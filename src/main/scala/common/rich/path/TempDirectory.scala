package common.rich.path

import java.io.File

import scala.util.Random

import common.rich.path.RichFile.poorFile

/**
 * a directory that is deleted on exit. All subfiles and directories created for this directory are
 * also deleted on exit
 */
class TempDirectory(f: File) extends Directory(f: File) {
  f.deleteOnExit()

  override def addSubDir(name: String) =
    TempDirectory(super.addSubDir(name).dir)
  override def addFile(name: String) = {
    val $ = super.addFile(name)
    $.deleteOnExit()
    $
  }

  def addFile(): RichFile = addFile(Random.nextLong().toString)
}

object TempDirectory {
  private def apply(f: File) = new TempDirectory(f)
  def apply(): TempDirectory = {
    val f = File.createTempFile("temp", "dir")
    f.delete
    f.mkdirs
    apply(f)
  }
}
