package common.os

import java.io.File

import common.rich.path.RichFile.richFile
import common.rich.path.ref.io.IODirectory

trait RichOs {
  def getAssociation(file: File): String
  def getRunningProcesses: Seq[ProcessInfo]
  def kill(pid: Int): Unit
  def unzip(file: File): Unit = unzip(file, outputDir = file.parent)
  def unzip(file: File, outputDir: IODirectory): Unit
}

object RichOs {
  private val os = System.getProperty("os.name")
  def get: RichOs =
    if (os.toLowerCase.contains("windows"))
      RichWindows
    else if (os.toLowerCase.contains("linux"))
      RichLinux
    else
      throw new MatchError(os)
}
