package common.path.ref.io

import java.io.{File, IOException}

import common.path.ref.PathRefFactory

object IOPathRefFactory extends PathRefFactory {
  override def parsePath(path: String): IOPath = {
    val file = new File(path)
    if (file.isDirectory)
      IODirectory(file)
    else if (file.isFile)
      IOFile(file)
    else
      throw new IOException(s"Path <$path> is not a valid file or directory")
  }
  override def parseFilePath(path: String): IOFile = IOFile(path)
  override def parseFilePathUnsafe(path: String): IOFile = IOFile.unsafe(path)
  override def parseDirPath(path: String): IODirectory = IODirectory(path)
  override def parseDirPathUnsafe(path: String): IODirectory = IODirectory.unsafe(path)
}
