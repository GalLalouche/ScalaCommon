package common.test.memory_ref

import java.nio.file.attribute.BasicFileAttributes

import common.path.ref.PathRef
import common.rich.primitives.RichOption.richOption

trait MemoryPath extends PathRef {
  override type S = MemorySystem
  private[memory_ref] def packageWitness: PackageWitness
  override def /(name: String): MemoryPath =
    this
      .asInstanceOf[MemoryDir]
      .getSubPath(name)
      .getOrThrow(s"No such subpath <$name> in parent <$this>")
  override def exists: Boolean = parent.files.exists(_.name == this.name)
  private[memory_ref] def basicFileAttributes: BasicFileAttributes = new BasicFileAttributes {
    override def isRegularFile: Boolean = false
    override def isOther: Boolean = false
    override def size(): Long = 0L
    override def lastModifiedTime = FileTimeUtils.from(MemoryPath.this.lastModifiedTime)
    override def creationTime = FileTimeUtils.from(MemoryPath.this.creationTime)
    override def isDirectory: Boolean = true
    override def lastAccessTime = lastModifiedTime
    override def isSymbolicLink = false
    override def fileKey() = MemoryPath.this
  }
}
