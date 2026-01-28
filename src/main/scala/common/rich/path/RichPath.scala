//package common.rich.path
//
//import java.io.File
//import java.nio.file.Path
//
//import better.files.{File => BFile, FileExtensions}
//
//import scala.annotation.tailrec
//import scala.collection.mutable.ArrayBuffer
//import scala.language.implicitConversions
//
//import common.rich.path.ref.io.IODirectory
//
//class RichPath protected (protected val f: File) {
//  def better: BFile = f.toScala
//
//  override def equals(obj: Any): Boolean = obj match {
//    // Fix for MacOS (of course).
//    case otherF: RichPath => f.getCanonicalPath == otherF.f.getCanonicalPath
//    case _ => false
//  }
//  override lazy val hashCode: Int = f.getCanonicalPath.hashCode()
//
//  def path: String = f.getCanonicalPath.replaceAll("\\\\", "/")
//  def name: String = f.getName // including its extension
//
//  def /(s: String): RichPath = {
//    val f = new File(path + "/" + s)
//<<<<<<< HEAD
//    if (f.isDirectory) new Directory(f) else new RichFile.richFile(f)
//=======
//    if (f.isDirectory) IODirectory(f) else new RichFile(f)
//>>>>>>> a17ba37 (TEMP)
//  }
//  def \(s: String): File = new File(path + "/" + s)
//  def \(): File = RichPath.this \ ""
//
//  def / : IODirectory = IODirectory(f)
//
//  override def toString = path
//
//  def parent: IODirectory = {
//    // this has to be lazy, to avoid computing entire path to root in construction
//    if (f.getParentFile == null)
//      throw new UnsupportedOperationException(s"File: $f has no parent")
//    IODirectory(f.getParentFile)
//  }
//
//  def parents: Seq[IODirectory] = {
//    @tailrec
//    def go(xs: ArrayBuffer[IODirectory], rp: RichPath): Seq[IODirectory] =
//      if (rp.f.getParentFile == null) xs.toVector else go(xs += rp.parent, rp.parent)
//    go(ArrayBuffer(), RichPath.this)
//  }
//
//  def toPath: Path = f.toPath
//}
//
//object RichPath {
//  implicit def richPath(file: File): RichPath = {
//    require(file.exists())
//    new RichPath(file)
//  }
//
//  implicit def poorPath(rp: RichPath): File = rp.f
//}
