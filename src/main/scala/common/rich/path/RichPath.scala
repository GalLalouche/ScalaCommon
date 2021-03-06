package common.rich.path

import java.io.File
import java.nio.file.{FileAlreadyExistsException, Path}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

abstract case class RichPath[T <: RichPath[T]] protected(f: File) {
  require(f.getAbsoluteFile.exists, f.getAbsolutePath + " doesn't exist")

  override def equals(obj: Any): Boolean = obj match {
    // Fix for MacOS (of course).
    case RichPath(otherF) => f.getCanonicalPath == otherF.getCanonicalPath
    case _ => false
  }

  val path: String = f.getCanonicalPath.replaceAll("\\\\", "/")
  val name: String = f.getName // including its extension

  def /(s: String): RichPath[_] = {
    val f = new File(path + "/" + s)
    if (f.isDirectory) Directory(f) else new RichFile(f)
  }
  def \(s: String): File = new File(path + "/" + s)
  def \(): File = RichPath.this \ ""

  def / = new Directory(f)

  override def toString = path

  def parent: Directory = {
    // this has to be lazy, to avoid computing entire path to root in construction
    if (f.getParentFile == null)
      throw new UnsupportedOperationException(s"File: $f has no parent")
    Directory(f.getParentFile)
  }

  def parents: Seq[Directory] = {
    @tailrec
    def go(xs: ArrayBuffer[Directory], rp: RichPath[_]): Seq[Directory] =
      if (rp.f.getParentFile == null) xs.toVector else go(xs += rp.parent, rp.parent)
    go(ArrayBuffer(), RichPath.this)
  }

  protected def internalCopyTo(f: File): T
  /**
   * Copies this path to another location with the same name.
   *
   * @throws FileAlreadyExistsException if a file (or directory) with the same name already exists in the destination
   */
  def copyTo(dstDir: Directory): T = copyTo(dstDir, name)
  /**
   * Copies this file to another location with the same name.
   *
   * @throws FileAlreadyExistsException if a file (or directory) with the same name already exists in the destination
   */
  def copyTo(dstDir: Directory, newName: String): T = {
    val dstFile = dstDir \ newName
    if (dstFile.exists())
      throw new FileAlreadyExistsException(dstFile.getPath)
    internalCopyTo(dstFile)
  }

  def toPath: Path = f.toPath
}

object RichPath {
  implicit def poorPath(rp: RichPath[_]): File = rp.f
}
