package common.rich.path

import java.io.File
import java.nio.file.{FileAlreadyExistsException, Files}

import common.rich.path.RichFile._

/**
 * Several utility functions that move files and directories. These aren't implemented as methods as in RichFile and
 * Directory since those classes assume their input file or directory exists.
 */
//TODO figure out a way to make Directory/RichFile safer... This would be a breaking change so it would probably involve changing the version
object RichFileUtils {
  def move(src: File, dst: Directory): RichFile = move(src, dst, src.name)

  /** Moves a file to another directory. */
  def move(src: File, dst: Directory, newName: String): RichFile = {
    Files.move(src.toPath, (dst \ newName).toPath).toFile
  }
  /**
   * Moves a directory (including itself) to a parent directory, returning the new directory. This method does not
   * perform merging of any sort.
   * @throws FileAlreadyExistsException if a file (directory or actual) in the destination dir already exists with
   *                                    the same name as the src directory.
   */
  def move(src: Directory, parentDirectory: Directory): Directory = move(src, parentDirectory, src.name)

  def move(src: Directory, parentDirectory: Directory, newName: String): Directory = {
    Files.move(src.toPath, (parentDirectory \ newName).toPath)
    parentDirectory / src.name /
  }
  /**
   * Moves the contents of a directory (but not the directory itself) to another directory. This method does not perform
   * merging of any sort.
   * @throws FileAlreadyExistsException if a file (directory or actual) in the destination dir already exists with the
   *                                    same name as *any* file (directory or actual) in the source dir.
   */
  def moveContents(src: Directory, dst: Directory): Unit = {
    val dstFileNamesToFullPaths: Map[String, String] = dst.paths.map(e => e.name -> e.path).toMap

    for (srcPath <- src.paths)
      for (dstPathWithSameName <- dstFileNamesToFullPaths.get(srcPath.name))
        throw new FileAlreadyExistsException(srcPath.path, dstPathWithSameName, "File with same name already exists")

    src.paths.foreach {
      case d: Directory => move(d, dst)
      case f: RichFile => move(f, dst)
    }
  }
}
