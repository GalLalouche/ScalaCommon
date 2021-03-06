package common.rich.func

import scala.Ordering.Implicits._
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

import scalaz.{Applicative, Foldable, MonadError, Monoid, PlusEmpty}
import scalaz.syntax.foldable.ToFoldableOps

import common.rich.primitives.RichBoolean._
import common.rich.RichT._

trait ToMoreFoldableOps {
  implicit class toMoreFoldableOps[A, F[_] : Foldable]($: F[A]) {
    // because scalaz isn't tail recursive 🔔 shame 🔔 shame 🔔
    def doForEach(f: A => Unit): F[A] = $.<|(Foldable[F].foldl(_, ())(_ => e => f(e)))
    def printPerLine(): F[A] = doForEach(println)
    // Why isn't this in the scalaz library? Who knows
    def foldMapPE[M[_] : PlusEmpty, B](f: A => M[B]): M[B] =
      Foldable[F].foldMap($)(f)(PlusEmpty[M].monoid)
    def foldLeftME[M[_], B, S](f: A => M[B], b: M[B])(implicit ev: MonadError[M, S]): M[B] = {
      import ToMoreMonadErrorOps._
      Foldable[F].foldLeft($, b)(_ orElseTry f(_))
    }
    def mapHeadOrElse[B](f: A => B, default: => B): B = headOpt.fold(default)(f)
    def headOpt: Option[A] = $ index 0
    def head: A = headOpt.get

    /** O(n * log(k)), where n is the size of the foldable. */
    def topK(k: Int)(implicit ord: Ordering[A]): Seq[A] = {
      val q = new java.util.PriorityQueue[A](k, ord.compare)
      doForEach {next =>
        if (q.size < k)
          q.add(next)
        else if (q.peek < next) {
          q.poll()
          q.add(next)
        }
      }
      val mb = new ListBuffer[A]()
      while (q.isEmpty.isFalse)
        q.poll() +=: mb
      mb.toVector
    }
    def bottomK(k: Int)(implicit ord: Ordering[A]): Seq[A] = topK(k)(ord.reverse)
    def asum[M[_], B](implicit applicativeEv: A =:= M[B],
        applicative: Applicative[M],
        monoid: Monoid[B],
    ): M[B] =
      Foldable[F].foldLeft($, applicative.pure(monoid.zero))(applicative.apply2(_, _)(monoid.append(_, _)))
    // Doesn't collide with the standard library fold method for Traversables.
    def foldMonoid(implicit m: Monoid[A]): A = $.fold
  }
}

object ToMoreFoldableOps extends ToMoreFoldableOps
