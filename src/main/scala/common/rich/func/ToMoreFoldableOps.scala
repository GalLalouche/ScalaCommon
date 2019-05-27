package common.rich.func

import common.rich.func.ToMoreMonadErrorOps._
import common.rich.primitives.RichBoolean._
import scalaz.syntax.ToFoldableOps
import scalaz.{Foldable, MonadError, PlusEmpty}

import scala.Ordering.Implicits._
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

trait ToMoreFoldableOps extends ToFoldableOps {
  implicit class toMoreFoldableOps[A, F[_] : Foldable]($: F[A]) {
    def doForEach(f: A => Unit): F[A] = {
      // because scalaz isn't tail recursive 🔔 shame 🔔 shame 🔔
      var list: List[A] = Nil
      Foldable[F].any($) {e => list = e :: list; false}
      list foreach f
      $
    }
    def printPerLine(): F[A] = doForEach(println)
    // Why isn't this in the scalaz library? Who knows
    def foldMapPE[M[_] : PlusEmpty, B](f: A => M[B]): M[B] =
      Foldable[F].foldMap($)(f)(PlusEmpty[M].monoid)
    def foldLeftME[M[_], B, S](f: A => M[B], b: M[B])(implicit ev: MonadError[M, S]): M[B] =
      Foldable[F].foldLeft($, b)(_ orElseTry f(_))
    def mapHeadOrElse[B](f: A => B, default: => B): B = headOpt.fold(default)(f)
    def headOpt: Option[A] = $ index 0
    def head: A = headOpt.get

    /** O(n * log(k)), where n is the size of the foldable. */
    def topK(k: Int)(implicit ord: Ordering[A]): Seq[A] = {
      val q = new java.util.PriorityQueue[A](k, ord.compare)
      doForEach(next => {
        if (q.size < k)
          q.add(next)
        else if (q.peek < next) {
          q.poll()
          q.add(next)
        }
      })
      val mb = new ListBuffer[A]()
      while (q.isEmpty.isFalse)
        q.poll() +=: mb
      mb.toVector
    }
    def bottomK(k: Int)(implicit ord: Ordering[A]): Seq[A] = topK(k)(ord.reverse)
  }
}

object ToMoreFoldableOps extends ToMoreFoldableOps
