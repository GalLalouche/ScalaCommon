package common

import java.io.File

import common.rich.path.RichFile._

class InnerScalaScripter(val classPath: Seq[File]) {
  def forWindows (src: File, dst: File): File = {
    dst.clear()
    val classPathsString = s""""${classPath.mkString(";") }""""
    dst.write( s"""::#!
                 |  @echo off
                 |  call scala -cp $classPathsString %0 %*
                 |  goto :eof
                 |::!#""".stripMargin + "\n")
    dst.write(src.readAll)
    dst
  }
}

object ScalaScripter extends InnerScalaScripter(Vector.empty) {
  private val defaultCp = Vector(new File("D:\\tmp\\intellij output\\artifacts\\SoftwareMetrics.jar"))
  def withClassPath(cp: File, other: File*) = new InnerScalaScripter(cp :: other.toList)
}
