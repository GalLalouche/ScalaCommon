package common.rich.path.ref.io

import java.io.{File, FileNotFoundException, IOException}

import common.rich.path.ref.FileRef
import common.rich.primitives.RichBoolean.richBoolean

/** For production; actual, existing, files (not directories) on the disk. */
final class IOFile private (override val path: String) extends File(path) with IOPath with FileRef {
  // TODO avoid canonicalization here if possible.
  override def parent: IODirectory = IODirectory(getParent)
  private[io] override def witness: PackageWitness = PackageWitness
}

object IOFile {
  def apply(path: String): IOFile = IOFile(new File(path))
  def apply(file: File): IOFile = {
    if (file.exists.isFalse)
      throw new FileNotFoundException(s"File does not exist: <${file.getPath}>")
    if (file.isDirectory)
      throw new IOException(s"Expected a file but got a directory: <${file.getPath}>")
    new IOFile(file.getCanonicalPath)
  }
  @inline private[io] def unsafe(s: String): IOFile = unsafe(new File(s))
  @inline private[io] def unsafe(f: File): IOFile = new IOFile(f.getPath)
}
