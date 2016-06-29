package common.os

import java.io.File

import scala.sys.process.Process

object RichLinux extends RichOs {
  override def getAssociation(file: File): String = {
    val mimeType = Process("file --mime-type -b " + file.getCanonicalPath).!!
    Process("xdg-mime query default " + mimeType).!!
      .takeWhile(_ != '.')
  }
  
  override def getRunningProcesses: Seq[ProcessInfo] = {
    val userName = System getProperty "user.name"
    val pidsToCmd = {
      val ps = Process("ps").!!.split("\n")
      val indexOfCmd = ps.head.indexOf("CMD")
      ps
        .drop(1)
        .map(e => {
          val pid = e.trim.split("\\s+")(0).toInt
          val cmd = e.substring(indexOfCmd)
          pid -> cmd
        })
        .toMap
    }
    val pidsToFullCmd = {
      val ps = Process("ps aux").!!.split("\n")
      val indexOfCmd = ps.head.indexOf("COMMAND")
      ps
        .drop(1)
        .map(e => {
          val pid = e.dropWhile(_ != ' ').trim.split("\\s+")(0)
            .toInt
          val cmd = e.substring(indexOfCmd)
          pid -> cmd
        })
        .toMap
    }
    (for ((pid, cmd) <- pidsToCmd; if pidsToFullCmd.contains(pid))
      yield ProcessInfo(cmd, pidsToFullCmd(pid), pid)).toVector
  }
  
  override def kill(pid: Int) { Process("kill " + pid) }
}
