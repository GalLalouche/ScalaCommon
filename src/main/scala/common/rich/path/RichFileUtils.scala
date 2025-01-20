package common.rich.path

import java.io.File
import java.nio.file.{FileAlreadyExistsException, Files}

import common.rich.path.RichPath.richPath
import common.rich.primitives.RichBoolean._

/**
 * Utility functions for moving files and directories. These aren't implemented as methods in
 * RichFile and Directory since those classes assume their input file or directory exists.
 */
object RichFileUtils {
  /**
   * Moves a file to another directory, keeping the same name.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   the src file.
   */
  def move(src: File, dst: Directory): RichFile = move(src, dst, src.name)

  /**
   * Moves a file to another directory, giving it a new name.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   the requested new name for the file.
   */
  def move(src: File, dst: Directory, newName: String): RichFile = {
    require(
      src.isDirectory.isFalse,
      "Can't move directories pretending to be a File because java.nio is Stupid. Wrap with Directory first.",
    )
    Files.move(src.toPath, (dst \ newName).toPath).toFile
  }

  /**
   * Renames a file, keeping it in the same directory.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the parent dir already exists with the same name as the
   *   requested new name for the file.
   */
  def rename(src: File, newName: String): RichFile = move(src, src.parent, newName)

  /**
   * Moves a directory (including itself) to a parent directory, keeping its name, and returns the
   * new directory. This method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   the src directory.
   */
  def move(src: Directory, parentDirectory: Directory): Directory =
    move(src, parentDirectory, src.name)

  /**
   * Moves a directory (including itself) to a parent directory, with a new name, and returns the
   * new directory. This method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   requested new name for the directory.
   */
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
   * Renames a directory, keeping it in its parent directory, returning the renamed directory.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the parent directory already exists with the same name as
   *   requested new name for the directory.
   */
  def rename(src: Directory, newName: String): Directory = move(src, src.parent, newName)

  /**
   * Moves the contents of a directory (but *not* the directory itself) to another directory. This
   * method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   *any* file (directory or actual) in the source dir.
   */
  def moveContents(src: Directory, dst: Directory): Unit = {
    val dstFileNamesToFullPaths: Map[String, String] =
      dst.listFiles.map(e => e.name -> e.path).toMap

    for (srcPath <- src.listFiles)
      for (dstPathWithSameName <- dstFileNamesToFullPaths.get(srcPath.name))
        throw new FileAlreadyExistsException(
          srcPath.path,
          dstPathWithSameName,
          "File with same name already exists",
        )

    src.listFiles.foreach(f => if (f.isDirectory) move(Directory(f), dst) else move(f, dst))
  }
}
