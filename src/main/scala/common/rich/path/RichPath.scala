package common.rich.path

import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class RichPath protected(val p: File) {
  // this is required - if a path is used on a file that does not exist, it is considered a programming bug
  require(p.getAbsoluteFile exists, p.getAbsolutePath + " doesn't exist")

  val path = p.getCanonicalPath.replaceAll("\\\\", "/")
  val name = p.getName // including its extension

  def /(s: String): RichPath = {
    val f = new File(path + "/" + s)
    if (f.isDirectory) Directory(f) else new RichFile(f)
  }
  def \(s: String): File = new File(path + "/" + s)
  def \(): File = RichPath.this \ ""

  def / = new Directory(p)

  override def toString = name

  override def hashCode = p.hashCode
  override def equals(o: Any): Boolean = o match {
    case e: RichPath => e.p == this.p
    case _ => false
  }

  def parent: Directory = {
    // this has to be lazy, to avoid computing entire path to root in construction
    if (p.getParentFile == null)
      throw new UnsupportedOperationException(s"File: $p has no parent")
    Directory(p.getParentFile)
  }

  def parents: Seq[Directory] = {
    @tailrec
    def aux(xs: ListBuffer[Directory], p: RichPath): Seq[Directory] = {
      if (p.getParentFile == null) xs else aux(xs += p.parent, p.parent)
    }
    aux(ListBuffer[Directory](), RichPath.this)
  }

  def renameTo(newName: String) {
    p.renameTo(new File(parent, newName))
  }
}

object RichPath {
  implicit def richPath(f: File) = if (f.isDirectory) new Directory(f.getAbsoluteFile) else new RichFile(f)
  implicit def poorPath(p: RichPath): File = p.p
}
