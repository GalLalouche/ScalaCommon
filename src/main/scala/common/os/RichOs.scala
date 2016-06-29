package common.os

import java.io.File

trait RichOs {
  def getAssociation(file: File): String
  def getRunningProcesses: Seq[ProcessInfo]
  def kill(pid: Int): Unit
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
