package common.rich.func

import scalaz.syntax.ToFoldableOps
import scalaz.{Foldable, PlusEmpty}

trait ToMoreFoldableOps extends ToFoldableOps {
  implicit class richFoldable[A, F[_] : Foldable]($: F[A]) {
    private val foldable = implicitly[Foldable[F]]
    def doForEach(f: A => Unit): F[A] = {
      // because scalaz isn't tail recursive 🔔 shame 🔔 shame 🔔
      var list: List[A] = Nil
      implicitly[Foldable[F]].any($) { e => list = e :: list; false }
      list foreach f
      $
    }
    def printPerLine(): F[A] = doForEach(println)
    // Why isn't this in the scalaz library? Who knows
    def foldMapPE[M[_]: PlusEmpty, B](f: A => M[B]): M[B] = foldable.foldMap($)(f)(implicitly[PlusEmpty[M]].monoid)
  }
}

