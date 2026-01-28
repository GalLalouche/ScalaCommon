package common.path.ref.io

import java.io.{File, FileInputStream, FileOutputStream, InputStream, IOException, OutputStream}

import common.TestAsserts.testAssert
import common.path.ref.FileRef
import common.rich.RichFile

/** For production: actual, existing, files (not directories) on the disk. */
final class IOFile private (override val path: String) extends File(path) with IOPath with FileRef {
  override def parent: IODirectory = IODirectory.unsafe(getParent)
  private[io] override def witness: PackageWitness = PackageWitness
  @inline private def asRichFile: RichFile.richFile = RichFile.richFile(new File(path))
  override def size: Long = length
  override def bytes: Array[Byte] = asRichFile.bytes
  override def write(bs: Array[Byte]): IOFile = { asRichFile.write(bs); this }
  override def appendLine(line: String): IOFile = { asRichFile.appendLine(line); this }
  override def readAll: String = asRichFile.readAll
  override def outputStream: OutputStream = new FileOutputStream(this)
  override def inputStream: InputStream = new FileInputStream(this)
  override def exists: Boolean = super[File].exists
}

object IOFile {
  def apply(path: String): IOFile = apply(new File(path))
  def apply(path: IOPath): IOFile = apply(path.path)
  def apply(file: File): IOFile =
    if (file.isFile) new IOFile(file.getCanonicalPath)
    else throw new IOException(s"File <$file> is not a valid file")
  /** Does not check for existence or if the file is a directory, nor perform canonicalization. */
  @inline def unsafe(path: String): IOFile = {
    testAssert(new File(path).getCanonicalPath == path, s"IOFile path <$path> is not canonicalized")
    new IOFile(path)
  }
  /** Does not check for existence or if the file is a directory, nor perform canonicalization. */
  @inline def unsafe(f: File): IOFile = unsafe(f.getPath)
}
