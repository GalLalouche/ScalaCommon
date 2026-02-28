package common.test.memory_ref

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime

import common.path.ref.FileRef
import common.rich.primitives.RichString.richString

case class MemoryFile(parent: MemoryDir, name: String) extends FileRef with MemoryPath {
  private var content: Array[Byte] = new Array[Byte](0)
  private var lastUpdatedTime: LocalDateTime = LocalDateTime.now()
  private def touch(): Unit = lastUpdatedTime = LocalDateTime.now
  override def bytes = content
  override def write(bs: Array[Byte]): MemoryFile = { content = bs; this }
  override def write(s: String): MemoryFile = write(s.getBytes(StandardCharsets.UTF_8))
  override def appendLine(line: String) = {
    content ++= line.getBytes(StandardCharsets.UTF_8)
    touch()
    this
  }

  override def readAll: String = new String(content, StandardCharsets.UTF_8)
  override def inputStream: InputStream = readAll.toInputStream
  override def path: String = parent.path + "/" + name
  override def lastModifiedTime: LocalDateTime = lastUpdatedTime
  override def size: Long = bytes.length
  override def delete: Boolean = parent.deleteFile(this.name)

  override val creationTime: LocalDateTime = LocalDateTime.now()
  override def lastAccessTime: LocalDateTime = lastUpdatedTime
  override def outputStream: OutputStream = {
    // Matches FileOutputStream's default behavior of truncating the file on open.
    content = new Array[Byte](0)
    new OutputStream {
      override def write(b: Int): Unit = {
        MemoryFile.this.content :+= b.toByte
        touch()
      }
      // Overridden for performance: the default implementation calls write(int) in a loop, which
      // would be O(n^2) due to array copying on each byte.
      override def write(b: Array[Byte], off: Int, len: Int): Unit = {
        MemoryFile.this.content ++= b.slice(off, off + len)
        touch()
      }
    }
  }

  private[memory_ref] override def packageWitness: PackageWitness = PackageWitness
}
