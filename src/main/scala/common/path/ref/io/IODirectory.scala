package common.path.ref.io

import java.io.{File, IOException}
import java.nio.file.{DirectoryStream, FileAlreadyExistsException, Files, FileVisitResult, Path, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes

import rx.lang.scala.{Observable, Subscriber}

import common.TestAsserts.testAssert
import common.UtilsVersionSpecific
import common.path.ref.DirectoryRef
import common.rich.ConvertersVersionSpecific
import common.rich.RichT.richT
import common.rich.primitives.RichBoolean._
import common.rich.primitives.RichString.richString

/** For production: actual, existing, directories on the disk. */
class IODirectory private[io] (override val path: String)
    extends File(path)
    with DirectoryRef
    with IOPath {
  /** Adds a new file under the directory if one doesn't exist, and returns it. */
  def addFile(name: String): IOFile = {
    val f = new File(this, name)
    if (f.isDirectory)
      throw new FileAlreadyExistsException(
        s"Could not create file <$name> under <$path>: a directory with the same name exists",
      )
    if (f.createNewFile)
      // Unfortunately, due to
      IOFile.unsafe(f)
    else
      // If the file already existed, we need to make sure it may not be canonical due to some OSs
      // being case-insensitive, e.g., addFile("t") when there's a file called "T".
      IOFile(f)
  }
  /** Adds a new sub-directory under this directory if one doesn't exist, and returns it. */
  def addSubDir(name: String): IODirectory = {
    val file = new File(this, name)
    if (file.exists)
      if (file.isDirectory.isFalse)
        throw new FileAlreadyExistsException(
          s"Could not create directory <$name> under <$path>: a file with the same name exists",
        )
      else
        // Same reason as above: if the directory already existed, we need to make sure the input
        // name is canonical with the existing one.
        IODirectory(file)
    else {
      assert(file.mkdirs())
      IODirectory.unsafe(file)
    }
  }

  /** Returns all direct sub-directory of this directory. */
  override def dirs: Iterator[IODirectory] =
    // Files.find includes the root dir itself, so we drop(1).
    fileIterator(_.isDirectory).drop(1).map(IODirectory unsafe _.toString)

  /** All direct files of this directory, that are *not* directories */
  override def files: Iterator[IOFile] = fileIterator(_.isRegularFile).map(IOFile unsafe _.toString)

  private def fileIterator(predicate: BasicFileAttributes => Boolean): Iterator[Path] =
    Files // Using Files.find to avoid fetching the file attributes multiple times.
      .find(this.toPath, 1, (_, attrs) => predicate(attrs))
      .iterator
      .|>(ConvertersVersionSpecific.toScala(_))

  /** Deletes all files and directories in this dir recursively including itself. */
  def deleteAll(): Unit = {
    def go(d: IODirectory): Unit = {
      d.dirs.foreach(go)
      d.files.foreach(x => if (x.exists && x.delete.isFalse) println("could not delete: " + x))
      if (d.exists && d.delete.isFalse)
        println("could not delete: " + d)
    }
    go(this)
  }
  /** Deletes all files and directories in this dir recursively <b>not</b> including itself. */
  def clear(): IODirectory = {
    files.foreach(_.delete)
    dirs.foreach(_.deleteAll())
    this
  }
  override def deepFiles: Iterator[IOFile] = deepPaths.collect { case f: IOFile => f }
  /** Significantly faster than the above iterator (at least on Windows). */
  def deepFilesObservable: Observable[(IOFile, BasicFileAttributes)] = observable { sub =>
    new EvenSimplerVisitor[Path](sub) {
      protected override def onFile(file: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((IOFile.unsafe(file.toString), attrs))
    }
  }
  /** Returns all directories nested inside this directory (in any given depth). */
  override def deepDirs: Iterator[IODirectory] = deepPaths.collect { case d: IODirectory => d }
  /** Significantly faster than the above iterator (at least on Windows). */
  override def deepDirsObservable: Observable[(IODirectory, BasicFileAttributes)] = observable {
    sub =>
      new EvenSimplerVisitor[IODirectory](sub) {
        protected override def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit =
          sub.onNext((IODirectory.unsafe(dir.toString), attrs))
      }
  }

  /** Returns all files and directories nested inside this directory (in any given depth). */
  def deepPaths: Iterator[IOPath] = listFiles.iterator.flatMap { f =>
    if (f.isDirectory) {
      val dir = IODirectory.unsafe(f)
      Iterator(dir) ++ dir.deepPaths
    } else
      Iterator(IOFile.unsafe(f))
  }
  /** Significantly faster than the above iterator (at least on Windows). */
  def deepPathsObservable: Observable[(File, BasicFileAttributes)] = observable(sub =>
    new EvenSimplerVisitor[Path](sub) {
      protected override def onFile(file: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((IOFile.unsafe(file.toString), attrs))
      protected override def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((IODirectory.unsafe(dir.toString), attrs))
    },
  )

  private def observable[A](
      fromSub: Subscriber[(A, BasicFileAttributes)] => SimpleFileVisitor[Path],
  ): Observable[(A, BasicFileAttributes)] = Observable { sub =>
    try {
      Files.walkFileTree(toPath, fromSub(sub))
      sub.onCompleted()
    } catch {
      case e: Throwable => sub.onError(e)
    }
  }

  private abstract class EvenSimplerVisitor[A](sub: Subscriber[_]) extends SimpleFileVisitor[Path] {
    protected def onFile(file: Path, attrs: BasicFileAttributes): Unit = {}
    protected def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit = {}
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (file ne toPath)
        onFile(file, attrs)
      result
    }
    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (dir ne IODirectory.this.toPath)
        onDirectory(dir, attrs)
      result
    }
    override def visitFileFailed(file: Path, exc: IOException) = {
      sub.onError(exc)
      super.visitFileFailed(file, exc)
    }
    private def result: FileVisitResult =
      if (sub.isUnsubscribed) FileVisitResult.TERMINATE else FileVisitResult.CONTINUE
  }
  override def getFile(name: String): Option[IOFile] = {
    val f = new File(this, name)
    if (f.exists.isFalse)
      return None
    if (f.isDirectory)
      throw new IOException(s"Expected a file but got a directory: <${f.getPath}>")
    Some(IOFile.unsafe(f))
  }
  override def getDir(name: String): Option[IODirectory] = {
    val file = new File(this, name)
    if (file.exists && file.isDirectory.isFalse)
      throw new IOException(s"Expected a directory but got a file: <${file.getPath}>")
    val d = IODirectory.unsafe(file)
    if (d.exists && d.isDirectory) Some(d) else None
  }
  override def getSubPath(name: String): Option[IOPath] = {
    val file = new File(this, name)
    if (file.exists) Some(if (file.isDirectory) IODirectory(file) else IOFile(file))
    else None
  }
  override def hasParent: Boolean = getParent != null
  override def parent: IODirectory = {
    val p = getParentFile
    require(p != null, s"Directory $path does not have a parent")
    IODirectory.unsafe(p)
  }
  private[io] override def witness: PackageWitness = PackageWitness
  override def containsFileWithExtension(extensions: Iterable[String]): Boolean = {
    val filter: DirectoryStream.Filter[Path] = entry => {
      val name = entry.getFileName.toString
      extensions.exists(name.endsWithCaseInsensitive)
    }
    UtilsVersionSpecific.using(Files.newDirectoryStream(toPath, filter))(_.iterator.hasNext)
  }
  override def exists: Boolean = super[File].exists
}

object IODirectory {
  def apply(f: File): IODirectory =
    if (f.isDirectory) new IODirectory(f.getCanonicalPath)
    else throw new IOException(s"File <$f> is not a valid directory")
  def apply(s: String): IODirectory = apply(new File(s))
  def apply(path: IOPath): IODirectory = apply(path.path)
  /** Does not check for existence or if the file is a directory, nor perform canonicalization. */
  @inline def unsafe(path: String): IODirectory = {
    testAssert(
      new File(path).getCanonicalPath == path,
      s"IODirectory path <$path> is not canonicalized",
    )
    new IODirectory(path)
  }
  /** Does not check for existence or if the file is a directory, nor perform canonicalization. */
  @inline def unsafe(f: File): IODirectory = unsafe(f.getPath)
  /** Creates all directories along the path as needed */
  def makeDir(f: File): IODirectory = {
    if (f.isDirectory.isFalse)
      require(f.mkdirs(), "Could not create directories path " + f)
    IODirectory(f)
  }

  def makeDir(fullPath: String): IODirectory = makeDir(new File(fullPath))
}
