package common.rich.path

import java.io.File
import java.nio.file.FileAlreadyExistsException

import cats.data.State
import cats.implicits.{catsSyntaxFlatMapOps, toFunctorOps}
import cats.syntax.traverse.toTraverseOps
import rx.lang.scala.Observer

import common.rich.func.kats.{PlainSeqInstances, RichState}
import common.rich.func.kats.RichState.richState
import common.rich.func.kats.ToMoreFunctorOps.toMoreFunctorOps

import common.rich.path.RichPath.richPath
import common.rx.report.ReportObserver

/** Like [[RichFileUtils]], but with observable progress reporting. */
// PlainSeqInstances extension needed for 2.12 source backward-compatibility.
object ObservableRichFileUtils extends PlainSeqInstances {
  /** The `total` may change when traversing new directories. */
  case class MoveFileProgress(current: File, processed: Int, total: Int)
  type Obs[A] = ReportObserver[MoveFileProgress, A]
  /** See the equivalent method in [[RichFileUtils]]. */
  def move(src: Directory, parentDirectory: Directory, obs: Obs[Directory]): Unit =
    move(src, parentDirectory, src.name, obs)

  /** See the equivalent method in [[RichFileUtils]]. */
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
      obs.toObserver {
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
  /** See the equivalent method in [[RichFileUtils]]. */
  def moveContents(
      src: Directory,
      dst: Directory,
      obs: Observer[MoveFileProgress],
  ): Unit = {
    def aux(src: Directory, dst: Directory): State[InternalProgress, Unit] = {
      def moveFile(f: File): State[InternalProgress, Unit] = {
        RichFileUtils.move(f, dst)
        RichState
          .updateAndGet[InternalProgress](_.incrementProcessed)
          .map(obs onNext _.toMoveProgress(f))
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
        aux(d, dst.addSubDir(d.name)) >| {
          assert(d.listFiles.isEmpty)
          d.delete().ensuring(e => e)
        }
      State
        .modify[InternalProgress](_.increaseTotal(srcFiles.size))
        .>>(srcFiles.traverse(f => if (f.isDirectory) moveDir(Directory(f)) else moveFile(f)))
        .void
    }
    aux(src, dst).exec(InternalProgress(0, 0))
    obs.onCompleted()
  }
}
