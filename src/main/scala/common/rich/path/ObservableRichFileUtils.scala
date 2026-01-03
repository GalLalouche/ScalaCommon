package common.rich.path

import java.io.File
import java.nio.file.FileAlreadyExistsException

import cats.data.State
import cats.syntax.traverse.toTraverseOps

import common.rich.func.kats.{PlainSeqInstances, RichState}
import common.rich.func.kats.RichState.richState
import common.rich.func.kats.ToMoreFunctorOps.toMoreFunctorOps

import common.rich.path.RichPath.richPath
import common.rx.report.ReportObserver

/**
 * Utility functions for moving files and directories. These aren't implemented as methods in
 * RichFile and Directory since those classes assume their input file or directory exists.
 */
// PlainSeqInstances extension needed for 2.12 source backward-compatibility.
object ObservableRichFileUtils extends PlainSeqInstances {
  case class MoveFileProgress(current: File, processed: Int, total: Int)
  type Obs[A] = ReportObserver[MoveFileProgress, A]
  /**
   * Moves a directory (including itself) to a parent directory, keeping its name, and returns the
   * new directory. This method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   the src directory.
   */
  def move(src: Directory, parentDirectory: Directory, obs: Obs[Directory]): Unit =
    move(src, parentDirectory, src.name, obs)

  /**
   * Moves a directory (including itself) to a parent directory, with a new name, and returns the
   * new directory. This method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   requested new name for the directory.
   */
  def move(
      src: Directory,
      parentDirectory: Directory,
      newName: String,
      obs: Obs[Directory],
  ): Unit = {
    if ((parentDirectory \ newName).exists)
      throw new FileAlreadyExistsException(parentDirectory.path + "/" + newName)
    val targetDir = parentDirectory.addSubDir(newName)
    moveContents(
      src,
      targetDir,
      obs.voidResult {
        assert(src.listFiles.isEmpty)
        src.deleteAll()
        targetDir
      },
    )
  }

  private case class InternalProgress(processed: Int, total: Int) {
    def incrementProcessed: InternalProgress = copy(processed = processed + 1)
    def increaseTotal(by: Int): InternalProgress = copy(total = total + by)
    def toMoveProgress(current: File) = MoveFileProgress(current, processed, total)
  }
  /**
   * Moves the contents of a directory (but *not* the directory itself) to another directory. This
   * method does not perform merging of any sort.
   * @throws FileAlreadyExistsException
   *   if a file (directory or actual) in the destination dir already exists with the same name as
   *   *any* file (directory or actual) in the source dir.
   */
  def moveContents(
      src: Directory,
      dst: Directory,
      obs: Obs[Unit],
  ): Unit = {
    def aux(
        src: Directory,
        dst: Directory,
        obs: Obs[Unit],
    ): State[InternalProgress, Unit] = {
      def moveFile(f: File): State[InternalProgress, Unit] = {
        RichFileUtils.move(f, dst)
        RichState
          .updateAndGet[InternalProgress](_.incrementProcessed)
          .map(obs onStep _.toMoveProgress(f))
      }

      val dstFileNamesToFullPaths: Map[String, String] =
        dst.listFiles.map(e => e.name -> e.path).toMap

      val srcFiles = src.listFiles
      for (srcPath <- srcFiles)
        for (dstPathWithSameName <- dstFileNamesToFullPaths.get(srcPath.name))
          throw new FileAlreadyExistsException(
            srcPath.path,
            dstPathWithSameName,
            "File with same name already exists",
          )

      def moveDir(d: Directory): State[InternalProgress, Unit] =
        aux(d, dst.addSubDir(d.name), obs) >| {
          assert(d.listFiles.isEmpty)
          d.delete().ensuring(e => e)
        }
      for {
        _ <- State.modify[InternalProgress](_.increaseTotal(srcFiles.size))
        _ <- srcFiles.traverse(f => if (f.isDirectory) moveDir(Directory(f)) else moveFile(f))
      } yield {}
    }
    aux(src, dst, obs).exec(InternalProgress(0, 0))
    obs.onComplete(())
  }
}
