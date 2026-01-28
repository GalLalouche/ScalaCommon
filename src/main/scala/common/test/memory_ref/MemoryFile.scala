package common.test.memory_ref

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
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
  override def outputStream: OutputStream = new ByteArrayOutputStream {
    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      MemoryFile.this.content ++= b.slice(off, off + len)
      touch()
    }
  }

  private[memory_ref] override def packageWitness: PackageWitness = PackageWitness
}
