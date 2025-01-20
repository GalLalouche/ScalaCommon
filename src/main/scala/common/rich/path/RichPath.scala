package common.rich.path

import java.io.File
import java.nio.file.Path

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class RichPath protected (protected val f: File) {
  override def equals(obj: Any): Boolean = obj match {
    // Fix for MacOS (of course).
    case otherF: RichPath => f.getCanonicalPath == otherF.f.getCanonicalPath
    case _ => false
  }
  override def hashCode(): Int = f.hashCode()

  def path: String = f.getCanonicalPath.replaceAll("\\\\", "/")
  def name: String = f.getName // including its extension

  def /(s: String): RichPath = {
    val f = new File(path + "/" + s)
    if (f.isDirectory) new Directory(f) else new RichFile(f)
  }
  def \(s: String): File = new File(path + "/" + s)
  def \(): File = RichPath.this \ ""

  def / : Directory = Directory(f)

  override def toString = path

  def parent: Directory = {
    // this has to be lazy, to avoid computing entire path to root in construction
    if (f.getParentFile == null)
      throw new UnsupportedOperationException(s"File: $f has no parent")
    Directory(f.getParentFile)
  }

  def parents: Seq[Directory] = {
    @tailrec
    def go(xs: ArrayBuffer[Directory], rp: RichPath): Seq[Directory] =
      if (rp.f.getParentFile == null) xs.toVector else go(xs += rp.parent, rp.parent)
    go(ArrayBuffer(), RichPath.this)
  }

  def toPath: Path = f.toPath
}

object RichPath {
  implicit def richPath(file: File): RichPath = {
    require(file.exists())
    new RichPath(file)
  }

  implicit def poorPath(rp: RichPath): File = rp.f
}
