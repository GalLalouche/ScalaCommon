package common.path.ref

import java.io.{File, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime

import common.rich.RichFile

/** Abstracts over files which are not directories. */
trait FileRef extends PathRef {
  type S <: RefSystem

  def size: Long
  def bytes: Array[Byte]

  def write(s: String): S#F = write(s.getBytes(StandardCharsets.UTF_8))
  def write(bs: Array[Byte]): S#F

  def clear(): FileRef = write("")
  def appendLine(line: String): S#F
  def readAll: String
  final def lines: Seq[String] = {
    // Splitting an empty string returns [""].
    val content = readAll
    if (content.isEmpty) Vector.empty else content.split("\n").toVector
  }

  def outputStream: OutputStream
  def inputStream: InputStream

  @inline private def asRichFile: RichFile.richFile = RichFile.richFile(new File(path))
  final def extension: String = asRichFile.extension
  @inline final def hasExtension(ext: String): Boolean = asRichFile.hasExtension(ext)
  @inline final def extensionIsAnyOf(exts: Iterable[String]): Boolean =
    asRichFile.extensionIsAnyOf(exts)
  @inline final def extensionIsAnyOf(str1: String, strs: String*): Boolean =
    asRichFile.extensionIsAnyOf(str1, strs: _*)
  @inline final def nameWithoutExtension: String = asRichFile.nameWithoutExtension

  def lastAccessTime: LocalDateTime

  def exists: Boolean
  def delete: Boolean

  override def parents: Seq[S#D] = parent +: parent.parents.asInstanceOf[Seq[S#D]]
  override def hasParent = true
}
