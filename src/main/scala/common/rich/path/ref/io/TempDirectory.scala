package common.rich.path.ref.io

import java.io.File

import scala.util.Random

/**
 * a directory that is deleted on exit. All subfiles and directories created for this directory are
 * also deleted on exit
 */
class TempDirectory private (_path: String) extends IODirectory(_path) {
  deleteOnExit()

  override def addSubDir(name: String) = TempDirectory(super.addSubDir(name))
  override def addFile(name: String) = {
    val $ = super.addFile(name)
    $.deleteOnExit()
    $
  }

  def addFile(): File = addFile(Random.nextLong().toString)

  override def toString = s"TempDirectory($path)"
}

object TempDirectory {
  private def apply(f: File) = new TempDirectory(f.getCanonicalPath)
  def apply(): TempDirectory = {
    val f = File.createTempFile("temp", "dir")
    f.delete
    f.mkdirs
    apply(f)
  }
}
