package common.rich.path.ref.io

import java.io.{File, FileNotFoundException, IOException}

import common.TestAsserts.testAssert
import common.rich.path.ref.FileRef
import common.rich.primitives.RichBoolean.richBoolean

/** For production: actual, existing, files (not directories) on the disk. */
final class IOFile private (override val path: String) extends File(path) with IOPath with FileRef {
  testAssert(new File(path).getCanonicalPath == path)
  override def parent: IODirectory = IODirectory.unsafe(getParent)
  private[io] override def witness: PackageWitness = PackageWitness
}

object IOFile {
  def apply(path: String): IOFile = apply(new File(path))
  def apply(file: File): IOFile = {
    if (file.exists.isFalse)
      throw new FileNotFoundException(s"File does not exist: <${file.getPath}>")
    if (file.isDirectory)
      throw new IOException(s"Expected a file but got a directory: <${file.getPath}>")
    new IOFile(file.getCanonicalPath)
  }
  /** Does not check for existence or that the file is a directory, nor perform canonicalization. */
  @inline private[io] def unsafe(s: String): IOFile = new IOFile(s)
  @inline private[io] def unsafe(f: File): IOFile = unsafe(f.getPath)
}
