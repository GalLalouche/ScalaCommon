package common.rich.path.ref.io

import java.io.{File, FileNotFoundException, IOException}
import java.nio.file.{DirectoryStream, FileAlreadyExistsException, Files, FileVisitResult, Path, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes

import rx.lang.scala.{Observable, Subscriber}

import scala.util.Using

import common.rich.path.ref.DirectoryRef
import common.rich.primitives.RichBoolean._
import common.rich.primitives.RichString.richString

/** Helper class for Directory methods. */
class IODirectory private[io] (override val path: String)
    extends File(path)
    with DirectoryRef
    with IOPath {
  /** Adds a new file under the directory if one doesn't exist, and returns it. */
  def addFile(name: String): IOFile = {
    // TODO figure out if we can skip the canonicalization here.
    val f = new File(this, name)
    if (f.isDirectory)
      throw new FileAlreadyExistsException(
        s"Could not create file <$name> under <$path>: a directory with the same name exists",
      )
    f.createNewFile
    IOFile(f)
  }
  /** Adds a new sub-directory under this directory if one doesn't exist, and returns it. */
  def addSubDir(name: String): IODirectory = {
    val file = new File(this, name)
    if (file.exists && file.isDirectory.isFalse)
      throw new FileAlreadyExistsException(
        s"Could not create directory <$name> under <$path>: a file with the same name exists",
      )
    // TODO figure out if we can skip the canonicalization here.
    if (file.exists.isFalse)
      file.mkdirs()
    IODirectory(file)
  }

  // TODO replace these with DirectoryStream
  // TODO avoid the canonicalization cost if possible
  /** Returns all direct sub-directory of this directory. */
  def dirs: Iterator[IODirectory] = listFiles.iterator.filter(_.isDirectory).map(IODirectory(_))

  /** All direct files of this directory, that are *not* directories */
  def files: Iterator[IOFile] = listFiles.iterator.filterNot(_.isDirectory).map(IOFile(_))

  /** Deletes all files and directories in this dir recursively including itself. */
  def deleteAll() {
    def go(d: IODirectory) {
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
  def deepFilesObservable: Observable[(File, BasicFileAttributes)] = observable { sub =>
    new EvenSimplerVisitor[Path](sub) {
      protected override def onFile(file: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((file.toFile, attrs))
    }
  }
  /** Returns all directories nested inside this directory (in any given depth). */
  override def deepDirs: Iterator[IODirectory] = deepPaths.collect { case d: IODirectory => d }
  /** Significantly faster than the above iterator (at least on Windows). */
  override def deepDirsObservable: Observable[(IODirectory, BasicFileAttributes)] = observable {
    sub =>
      new EvenSimplerVisitor[IODirectory](sub) {
        protected override def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit =
          // TODO avoid canonicalization here if possible
          sub.onNext((IODirectory(dir.toFile), attrs))
      }
  }

  /** Returns all files and directories nested inside this directory (in any given depth). */
  def deepPaths: Iterator[IOPath] = listFiles.iterator.flatMap { f =>
    if (f.isDirectory) {
      val dir = IODirectory(f)
      Iterator(dir) ++ dir.deepPaths
    } else
      Iterator(IOFile(f))
  }
  /** Significantly faster than the above iterator (at least on Windows). */
  def deepPathsObservable: Observable[(File, BasicFileAttributes)] = observable(sub =>
    new EvenSimplerVisitor[Path](sub) {
      protected override def onFile(file: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((IOFile(file.toFile), attrs))
      protected override def onDirectory(dir: Path, attrs: BasicFileAttributes): Unit =
        sub.onNext((IODirectory.unsafe(dir.toFile), attrs))
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

  override def toString = s"IODirectory($path)"

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
    // TODO avoid canonicalization here if possibl
    Some(IOFile(f))
  }
  override def getDir(name: String): Option[IODirectory] = {
    val file = new File(this, name)
    if (file.exists && file.isDirectory.isFalse)
      throw new IOException(s"Expected a directory but got a file: <${file.getPath}>")
    val d = IODirectory.unsafe(file)
    if (d.exists && d.isDirectory) Some(d) else None
  }
  override def hasParent: Boolean = getParent != null
  override def parent: IODirectory = {
    val p = getParentFile
    require(p != null, s"Directory $path does not have a parent")
    // TODO skip canonicalization if possible
    IODirectory(p)
  }
  private[io] override def witness: PackageWitness = PackageWitness
  override def containsFileWithExtension(extensions: Iterable[String]): Boolean = {
    val filter: DirectoryStream.Filter[Path] = entry => {
      val name = entry.getFileName.toString
      extensions.exists(name.endsWithCaseInsensitive)
    }
    Using.resource(Files.newDirectoryStream(toPath, filter))(_.iterator.hasNext)
  }
}

object IODirectory {
  def apply(f: File): IODirectory = {
    if (f.exists.isFalse)
      throw new FileNotFoundException(s"File does not exist: <${f.getPath}>")
    if (f.isDirectory.isFalse)
      throw new IOException(s"Expected a file but got a directory: <${f.getPath}>")
    new IODirectory(f.getCanonicalPath)
  }
  def apply(s: String): IODirectory = apply(new File(s))
  /** Does not check for existence or that the file is a directory. */
  @inline private[io] def unsafe(s: String): IODirectory = unsafe(new File(s))
  @inline private[io] def unsafe(f: File): IODirectory = new IODirectory(f.getCanonicalPath)
  /** Creates all directories along the path as needed */
  def makeDir(f: File): IODirectory = {
    if (f.isDirectory.isFalse)
      require(f.mkdirs(), "Could not create directories path " + f)
    IODirectory(f)
  }

  def makeDir(fullPath: String): IODirectory = makeDir(new File(fullPath))
}
