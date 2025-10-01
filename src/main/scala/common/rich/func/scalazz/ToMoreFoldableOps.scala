package common.rich.func.scalazz

import scala.collection.mutable.ArrayBuffer

import scalaz.{Applicative, Foldable, MonadError, Monoid, PlusEmpty}
import scalaz.syntax.foldable.ToFoldableOps

import common.rich.RichT._

trait ToMoreFoldableOps {
  implicit class toMoreFoldableOps[A, F[_]: Foldable]($ : F[A]) {
    // because scalaz isn't tail recursive 🔔 shame 🔔 shame 🔔
    // This uses foldr because some implementations of foldable, e.g., Set's, doesn't implement foldl
    // properly, which can cause an overflow.
    def doForEach(f: A => Unit): F[A] = $.<|(Foldable[F].foldl(_, ())(_ => f))
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
