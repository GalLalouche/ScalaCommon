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

  /** Moves file within the same directory. */
  def move(src: File, newName: String): RichFile = move(src, src.parent, newName)

  /**
   * Moves a directory (including itself) to a parent directory, returning the new directory. This method does not
   * perform merging of any sort.
   * @throws FileAlreadyExistsException if a file (directory or actual) in the destination dir already exists with
   *                                    the same name as the src directory.
   */
  def move(src: Directory, parentDirectory: Directory): Directory = move(src, parentDirectory, src.name)

  def move(src: Directory, parentDirectory: Directory, newName: String): Directory = {
    if ((parentDirectory \ newName).exists)
      throw new FileAlreadyExistsException(parentDirectory.path + "/" + newName)
    val targetDir = parentDirectory.addSubDir(newName)
    moveContents(src, targetDir)
    assert(src.listFiles.isEmpty)
    src.deleteAll()
    targetDir
  }
  /**
   * Moves the contents of a directory (but not the directory itself) to another directory. This method does not perform
   * merging of any sort.
   * @throws FileAlreadyExistsException if a file (directory or actual) in the destination dir already exists with the
   *                                    same name as *any* file (directory or actual) in the source dir.
   */
  def moveContents(src: Directory, dst: Directory): Unit = {
    val dstFileNamesToFullPaths: Map[String, String] = dst.listFiles.map(e => e.name -> e.path).toMap

    for (srcPath <- src.listFiles)
      for (dstPathWithSameName <- dstFileNamesToFullPaths.get(srcPath.name))
        throw new FileAlreadyExistsException(srcPath.path, dstPathWithSameName, "File with same name already exists")

    src.listFiles.foreach { f =>
      if (f.isDirectory) move(Directory(f), dst) else move(f, dst)
    }
  }
}
