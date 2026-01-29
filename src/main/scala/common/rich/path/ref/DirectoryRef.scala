package common.rich.path.ref

import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes

import rx.lang.scala.Observable

import common.UtilsVersionSpecific

/** Must exist. */
trait DirectoryRef extends PathRef { self =>
  type S <: RefSystem
  /** Throws if the name belongs to an existing directory. */
  def addFile(name: String): S#F
  /** Throws if the name is that of an existing directory. */
  def getFile(name: String): Option[S#F]
  /** Throws if the name belongs to an existing file (not a directory). */
  def addSubDir(name: String): S#D
  /** Throws if the name is that of an existing file (not a directory). */
  def getDir(name: String): Option[S#D]
  def dirs: Iterator[S#D]
  def files: Iterator[S#F]
  def containsFileWithExtension(extensions: Iterable[String]): Boolean
  def paths: Iterator[S#P] = dirs.++(files).asInstanceOf[Iterator[S#P]]
  def isDescendant(path: String): Boolean =
    Paths.get(path).normalize().startsWith(Paths.get(this.path).normalize())
  def deepDirs: Iterator[S#D]
  def deepDirsObservable: Observable[(S#D, BasicFileAttributes)]
  def deepFiles: Iterator[S#F]
  // TODO freaking unfoldables already
  override def parents =
    UtilsVersionSpecific
      .lazySeqIterate(Option(parent))(p =>
        if (p.get.hasParent) Some(p.get.parent.asInstanceOf[S#D]) else None,
      )
      .takeWhile(_.isDefined)
      .map(_.get)
  /** Returns all directories between this and dir. Throws if dir is not a parent of this. */
  def relativize(dir: S#D): Seq[S#D] = {
    val ps = parents.span(_ != dir)
    require(ps._2.nonEmpty, s"<$dir> is not a parent of <$this>")
    ps._1.toList
  }
  def clear(): DirectoryRef
}
