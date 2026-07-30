package common.rich

import java.io.{File, FileOutputStream, FileWriter}
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.time.{LocalDateTime, ZoneId}

import scala.io.Source
import scala.language.implicitConversions

import common.path.BackupFile
import common.path.ref.io.IODirectory
import common.rich.RichT._
import common.rich.primitives.RichOption.richOption
import common.rich.primitives.RichString.richString

// Not an implicit class since it is returned from some methods and it would be annoying if it was nested.
object RichFile {
  implicit class richFile(private val $ : File) extends AnyVal {
    def extension: String = {
      val i = $.getName.lastIndexOf('.')
      if (i == -1) "" else $.getName.substring(i + 1).toLowerCase
    }

    def nameWithoutExtension: String = $.getName.dropAfterLast('.')

    // TODO avoid this, since if we implement this in FileRef, we can avoid the canonicalization.
    def parent: IODirectory =
      Option($.getParentFile)
        .map(IODirectory(_))
        .getOrThrow(new UnsupportedOperationException("Root directories have no parent"))
    /** Case-insensitive. */
    def hasExtension(ext: String): Boolean = {
      val path = $.getPath
      path.length > ext.length + 1 &&
      path.charAt(path.length - ext.length - 1) == '.' &&
      path.endsWithCaseInsensitive(ext)
    }

    def extensionIsAnyOf(exts: Iterable[String]): Boolean = exts.exists(hasExtension)
    def extensionIsAnyOf(str1: String, strs: String*): Boolean =
      hasExtension(str1) || extensionIsAnyOf(strs)

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

    def lines: Seq[String] = {
      // only works for UTF-8... I'm so gonna pay for that some day :|
      def removeByteOrderMarkIfPresent(
          bytes: Array[Byte],
      ) = bytes.mapIf(_.take(3).toVector == Vector[Byte](-17, -69, -65)).to(_.drop(3))
      Source.fromBytes(removeByteOrderMarkIfPresent(bytes)).getLines().toVector
    }

    def bytes: Array[Byte] = Files.readAllBytes($.toPath)

    def hasSameContentAs(f: File): Boolean = f.bytes.sameElements(new richFile($).bytes)

    /** Returns a backup file of this file */
    def backup = new BackupFile($)

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
