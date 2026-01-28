package common.rich.path

import java.io.File
import java.nio.file.{FileAlreadyExistsException, Files}

import common.rich.path.RichFile.richFile
import common.rich.path.ref.io.IODirectory
import common.rich.primitives.RichBoolean._
import common.rx.RichObserver
import common.rx.report.ReportObserver

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
  def move(src: File, dst: IODirectory): File = move(src, dst, src.getName)

  /**
   * Moves a file to another directory, giving it a new name.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   the requested new name for the file.
   */
  def move(src: File, dst: IODirectory, newName: String): File = {
    require(
      src.isDirectory.isFalse,
      "Can't move directories pretending to be a File because java.nio is Stupid. Wrap with Directory first.",
    )
    Files.move(src.toPath, new File(dst, newName).toPath).toFile
  }

  /**
   * Renames a file, keeping it in the same directory.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the parent dir already exists with the same name as the
   *   requested new name for the file.
   */
  def rename(src: File, newName: String): File = move(src, src.parent, newName)

  /**
   * Moves a directory (including itself) to a parent directory, keeping its name, and returns the
   * new directory. This method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   the src directory.
   */
  def move(src: IODirectory, parentDirectory: IODirectory): IODirectory =
    move(src, parentDirectory, src.name)

  /**
   * Moves a directory (including itself) to a parent directory, with a new name, and returns the
   * new directory. This method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   requested new name for the directory.
   */
  def move(src: IODirectory, parentDirectory: IODirectory, newName: String): IODirectory =
    ReportObserver.asReturnValue[Any, IODirectory](
      ObservableRichFileUtils.move(
        src,
        parentDirectory,
        newName,
        _,
      ),
    )(RichObserver.noop)

  /**
   * Renames a directory, keeping it in its parent directory, returning the renamed directory.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the parent directory already exists with the same name as
   *   requested new name for the directory.
   */
  def rename(src: IODirectory, newName: String): IODirectory = move(src, src.parent, newName)

  /**
   * Moves the contents of a directory (but *not* the directory itself) to another directory. This
   * method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   *any* file (directory or actual) in the source dir.
   */
  def moveContents(src: IODirectory, dst: IODirectory): Unit =
    ObservableRichFileUtils.moveContents(src, dst, RichObserver.noop)
}
