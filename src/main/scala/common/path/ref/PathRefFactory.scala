package common.path.ref

trait PathRefFactory {
  def parsePath(path: String): PathRef
  def parseFilePath(path: String): FileRef
  /** Does not check for existence or if the file is a directory, nor perform canonicalization. */
  def parseFilePathUnsafe(path: String): FileRef
  def parseDirPath(path: String): DirectoryRef
  /** Does not check for existence or if the file is a directory, nor perform canonicalization. */
  def parseDirPathUnsafe(path: String): DirectoryRef
}
