package common.rich.path

import java.io.{File, FileOutputStream, FileWriter}
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.time.{LocalDateTime, ZoneId}

import scala.io.Source
import scala.language.implicitConversions

import common.rich.RichT._
import common.rich.path.ref.io.IODirectory
import common.rich.primitives.RichString.richString

// Not an implicit class since it is returned from some methods and it would be annoying if it was nested.
object RichFile {
  implicit class richFile(private val $ : File) {
    def extension: String = {
      val i = $.getName.lastIndexOf('.')
      if (i == -1) "" else $.getName.substring(i + 1).toLowerCase
    }

    def nameWithoutExtension: String = $.getName.dropAfterLast('.')

    // TODO avoid this, since if we implement this in FileRef, we can avoid the canonicalization.
    def parent: IODirectory = IODirectory($.getParentFile)

    /** Appends a line to the end of the file */
    def appendLine(s: String): File = {
      val fw = new FileWriter($, true)
      try fw.write(s + "\n")
      finally fw.close()
      $
    }

    /** Returns true iff the file is *totally* empty (i.e., not even blank lines) */
    def isEmpty: Boolean = lines.isEmpty

    /** Removes all data from the file */
    def clear(): File = {
      write(Array[Byte]())
      $
    }

    /** Writes the string to the file. This deletes all previous data in the file. */
    def write(s: String): File = write(s.getBytes)

    /** Writes the byte array to the file. This deletes all previous data in the file. */
    def write(bytes: Array[Byte]): File = {
      val fos = new FileOutputStream($)
      try fos.write(bytes)
      finally fos.close()
      $
    }

    /** Reads the entire content of the file as a single string */
    def readAll: String = lines.mkString("\n")

    /** Returns the lines of the file */
    def lines: Seq[String] = {
      // only works for UTF-8... I'm so gonna pay for that some day :|
      def removeByteOrderMarkIfPresent(
          bytes: Array[Byte],
      ) = bytes.mapIf(_.take(3).toVector == Vector[Byte](-17, -69, -65)).to(_.drop(3))
      Source.fromBytes(removeByteOrderMarkIfPresent(bytes)).getLines().toVector
    }

    /** Gets all bytes in the file */
    def bytes: Array[Byte] = Files.readAllBytes($.toPath)

    /**
     * Checks if this file has the same contents as another file
     *
     * @param f
     *   The file to compare with
     */
    def hasSameContentAs(f: File): Boolean = f.bytes.sameElements(new richFile($).bytes)

    /** Returns a backup file of this file */
    def backup = new BackupFile($)
    def /(path: String): File = new File($, path)

    def creationTime: LocalDateTime = fromFileTime(_.creationTime)
    def lastAccessTime: LocalDateTime = fromFileTime(_.lastAccessTime)
    def lastModifiedTime: LocalDateTime = fromFileTime(_.lastModifiedTime)
    private def fromFileTime(toTime: BasicFileAttributes => FileTime): LocalDateTime =
      LocalDateTime.ofInstant(
        toTime(Files.readAttributes($.toPath, classOf[BasicFileAttributes])).toInstant,
        ZoneId.systemDefault(),
      )
  }
}
