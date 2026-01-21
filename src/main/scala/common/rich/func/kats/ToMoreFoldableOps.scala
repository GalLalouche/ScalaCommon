package common.rich.func.kats

import cats.{Eval, Foldable}
import cats.syntax.foldable.toFoldableOps

trait ToMoreFoldableOps {
  implicit class toMoreFoldableOps[A, F[_]: Foldable]($ : F[A]) {
    def toVector: Vector[A] = $.toIterable.toVector
    def toSet: Set[A] = $.toIterable.toSet
    // Translated from the Haskell:
    // nth :: Foldable t => Int -> t a -> Maybe a
    // nth n _
    //   | n < 0     = Nothing
    // nth n xs = foldr step (\_ -> Nothing) xs n
    //   where
    //     step :: a -> (Int -> Maybe a) -> (Int -> Maybe a)
    //     step x r 0 = Just x
    //     step _ r k = r (k - 1)
    def nth(n: Int): Option[A] = {
      if (n < 0)
        return None
      type Stepper = Int => Eval[Option[A]]
      def step(x: A, r: Eval[Stepper]): Eval[Stepper] = {
        def aux(k: Int): Eval[Option[A]] =
          if (k == 0) Eval.now(Some(x))
          else r.flatMap(f => f(k - 1))

        Eval.now(aux)
      }
      val res: Eval[Stepper] = $.foldRight[Stepper](Eval.now(_ => Eval.now(None)))(step)
      res.map(_(n)).value.value
    }
    def headOpt: Option[A] = nth(0)
    def head: A = headOpt.get
    // Nicer type inference
    def mapHeadOrElse[B](f: A => B, default: => B): B = headOpt.fold(default)(f)
  }

  // Can't use ev A =:= Either[B, C], since the compiler can't infer those apparently...
  /** Equivalent to partitionMap(identity) */
  implicit class EitherFoldableOps[B, C, F[_]: Foldable]($ : F[Either[B, C]]) {
    def partitionEithers: (Seq[B], Seq[C]) = partitionEithers(-1, -1)
    def partitionEithers(leftSizeHint: Int, rightSizeHint: Int): (Seq[B], Seq[C]) = {
      val bs = Vector.newBuilder[B]
      val cs = Vector.newBuilder[C]
      if (leftSizeHint >= 0)
        bs.sizeHint(leftSizeHint)
      if (rightSizeHint >= 0)
        cs.sizeHint(rightSizeHint)
      $.foldMap[Unit] {
        case Left(b) => bs += b
        case Right(c) => cs += c
      }
      (bs.result(), cs.result())
    }
  }
}

object ToMoreFoldableOps extends ToMoreFoldableOps
