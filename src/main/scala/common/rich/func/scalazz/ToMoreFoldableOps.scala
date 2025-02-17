package common.rich.func.scalazz

import scala.Ordering.Implicits._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.higherKinds

import scalaz.{Applicative, Foldable, MonadError, Monoid, PlusEmpty}
import scalaz.syntax.foldable.ToFoldableOps

import common.rich.RichT._
import common.rich.primitives.RichBoolean._

trait ToMoreFoldableOps {
  implicit class toMoreFoldableOps[A, F[_]: Foldable]($ : F[A]) {
    // because scalaz isn't tail recursive 🔔 shame 🔔 shame 🔔
    def doForEach(f: A => Unit): F[A] = $.<|(Foldable[F].foldl(_, ())(_ => e => f(e)))
    def printPerLine(): F[A] = doForEach(println)
    // Why isn't this in the scalaz library? Who knows
    def foldMapPE[M[_]: PlusEmpty, B](f: A => M[B]): M[B] =
      Foldable[F].foldMap($)(f)(PlusEmpty[M].monoid)
    def foldLeftME[M[_], B, S](f: A => M[B], b: M[B])(implicit ev: MonadError[M, S]): M[B] = {
      import ToMoreMonadErrorOps._
      Foldable[F].foldLeft($, b)(_ orElseTry f(_))
    }
    def mapHeadOrElse[B](f: A => B, default: => B): B = headOpt.fold(default)(f)
    def headOpt: Option[A] = $.index(0)
    def head: A = headOpt.get

    /** O(n * log(k)), where n is the size of the foldable. */
    def topK(k: Int)(implicit ord: Ordering[A]): Seq[A] = {
      val q = new java.util.PriorityQueue[A](k, ord.compare)
      doForEach { next =>
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
    def asum[M[_], B](implicit
        applicativeEv: A =:= M[B],
        applicative: Applicative[M],
        monoid: Monoid[B],
    ): M[B] =
      Foldable[F].foldLeft($, applicative.pure(monoid.zero))(
        applicative.apply2(_, _)(monoid.append(_, _)),
      )
    // Doesn't collide with the standard library fold method for Traversables.
    def foldMonoid(implicit m: Monoid[A]): A = $.fold
  }

  // Can't use ev A =:= Either[B, C], since the compiler can't infer those apparently...
  // TODO this can be slightly more efficient if we knew the size of $ beforehand, so we could preallocate the
  //  array. For some collections, we get this for cheap, so it might make sense to implement this in RichSeq
  //  and check the type there?
  implicit class EitherFoldableOps[B, C, F[_]: Foldable]($ : F[Either[B, C]]) {
    def partitionEithers: (Seq[B], Seq[C]) = {
      val bs = new ArrayBuffer[B]()
      val cs = new ArrayBuffer[C]()
      $.doForEach {
        case Left(b) => bs += b
        case Right(c) => cs += c
      }
      (bs.toVector, cs.toVector)
    }
  }
}

object ToMoreFoldableOps extends ToMoreFoldableOps
