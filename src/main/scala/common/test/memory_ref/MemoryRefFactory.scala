package common.test.memory_ref

import common.path.ref.{DirectoryRef, FileRef, PathRefFactory}
import common.rich.primitives.RichOption.richOption

class MemoryRefFactory(root: MemoryRoot) extends PathRefFactory {
  override def parsePath(path: String): MemoryPath = fromFilePath(path)
  override def parseFilePath(path: String): MemoryFile = parsePath(path) match {
    case file: MemoryFile => file
    case _: MemoryDir =>
      throw new IllegalArgumentException(s"Path $path is a directory, not a file")
  }
  override def parseDirPath(path: String): MemoryDir = parsePath(path) match {
    case dir: MemoryDir => dir
    case _: MemoryFile =>
      throw new IllegalArgumentException(s"Path $path is a file, not a directory")
  }

  private def fromFilePath(file: String): MemoryPath = {
    val hash = System.identityHashCode(root).toString
    file match {
      case MemoryRefFactory.RootRegex(rootHash) =>
        if (rootHash != hash)
          throw new IllegalArgumentException(s"Invalid root <$rootHash>, expected <$hash>")
        else
          root
      case MemoryRefFactory.PathRegex(rootHash, path) =>
        if (rootHash != hash)
          throw new IllegalArgumentException(s"Invalid root <$rootHash>, expected <$hash>")
        val paths = path.split("/")
        assert(paths.nonEmpty)
        paths.foldLeft(root: MemoryPath) { (d, s) =>
          d match {
            case dir: MemoryDir =>
              dir
                .getFile(s)
                .orElse(dir.getDir(s))
                .getOrThrow(s"Not path named <$s> found under <$dir>")
            case _: MemoryFile =>
              throw new IllegalArgumentException(
                s"<$d> is a file, yet there are still remaining path fragments",
              )
          }
        }
      case _ => throw new IllegalArgumentException(s"Invalid path <$file>")
    }
  }
  override def parseFilePathUnsafe(path: String): FileRef = parseFilePath(path)
  override def parseDirPathUnsafe(path: String): DirectoryRef = parseDirPath(path)
}

object MemoryRefFactory {
  def apply(root: MemoryRoot): MemoryRefFactory = new MemoryRefFactory(root)

  private val RootRegexStr = """root\((\d+)\)/"""
  private val PathRegex = s"$RootRegexStr/(.*)".r
  private val RootRegex = RootRegexStr.r
}
