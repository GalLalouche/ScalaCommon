package common.rich.path.ref.io

import java.io.File

import common.rich.path.ref.FileRef

/** For production; actual files on the disk */
final class IOFile private (override val path: String) extends File(path) with IOPath with FileRef {
  // TODO avoid canonicalization here if possible.
  override def parent: IODirectory = IODirectory(getParent)
  private[io] override def witness: PackageWitness = PackageWitness
}

object IOFile {
  def apply(path: String): IOFile = IOFile(new File(path))
  def apply(file: File): IOFile = new IOFile(file.getCanonicalPath)
  @inline private[io] def unsafe(s: String): IOFile = unsafe(new File(s))
  @inline private[io] def unsafe(f: File): IOFile = new IOFile(f.getPath)
}
