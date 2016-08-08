package common.rich.func

import scalaz.Foldable

object RichFoldable {
  implicit class richFoldable[A, F[A] : Foldable]($: F[A]) {
    def doForEach(f: A => Unit): F[A] = {
      implicitly[Foldable[F]].any($){e => f(e); false}
      $
    }
    def printPerLine(): F[A] = doForEach(println)
  }
}

