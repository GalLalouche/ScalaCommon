package common.rich.path

import java.io._
import java.nio.file.Files

import common.rich.RichT._

import scala.io.Source
import scala.language.implicitConversions

// Not an implicit class since it is returned from some methods and it would be annoying if it was nested.
class RichFile(f: File) extends RichPath[RichFile](f) {
  // this is required - if a RichFile is created for a file that is a directory, it is considered a programming bug
  // Why is this commented out??
  //require(f.isDirectory == false, s"Rich file $f cannot be a directory")
  lazy val extension: String = {
    val i = f.getName.lastIndexOf('.')
    if (i == -1) "" else f.getName.substring(i + 1).toLowerCase
  }

  lazy val nameWithoutExtension: String = name substring(0, name.length - extension.length - 1)

  /** Appends a line to the end of the file */
  def appendLine(s: String): RichFile = {
    val fw = new FileWriter(f, true)
    try fw.write(s + "\n")
    finally fw.close()
    this
  }

  /** Returns true iff the file is *totally* empty (i.e., not even blank lines) */
  def isEmpty: Boolean = lines.isEmpty

  /** Removes all data from the file */
  def clear(): RichFile = {
    write(Array[Byte]())
    this
  }

  /**
   * Writes the string to the file.
   * This deletes all previous data in the file.
   */
  def write(s: String): RichFile = write(s.getBytes)

  /**
   * Writes the byte array to the file.
   * This deletes all previous data in the file.
   */
  def write(bytes: Array[Byte]): RichFile = {
    val fos = new FileOutputStream(f)
    try fos write bytes
    finally fos.close()
    this
  }

  /** Reads the entire content of the file as a single string */
  def readAll: String = lines.mkString("\n")

  /** Returns the lines of the file */
  def lines: Seq[String] = {
    def removeByteOrderMarkIfPresent(bytes: Array[Byte]) = // only works for UTF-8... I'm so gonna pay for that some day :|
      bytes.mapIf(_.take(3).toList == List[Byte](-17, -69, -65)).to(_.drop(3))
    Source.fromBytes(bytes mapTo removeByteOrderMarkIfPresent).getLines().toVector
  }

  /** Gets all bytes in the file */
  def bytes: Array[Byte] = Files.readAllBytes(f.toPath)

  /**
   * Checks if this file has the same contents as another file
   *
   * @param f The file to compare with
   */
  def hasSameContentAs(f: File): Boolean = bytes sameElements new RichFile(f).bytes

  /** Returns a backup file of this file */
  def backup = new BackupFile(f)

  override protected def internalCopyTo(dstFile: File): RichFile = {
    Files.copy(this.toPath, dstFile.toPath)
    new RichFile(dstFile)
  }
}

object RichFile {
  implicit def poorFile(f: RichFile): File = f.f

  implicit def richFile(f: File): RichFile = new RichFile(f)

  def apply(f: File) = new RichFile(f)

  def apply(s: String): RichFile = new RichFile(new File(s))
}

