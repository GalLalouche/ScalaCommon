package common.rich.func

import scalaz.Foldable

object RichFoldable {
  implicit class richFoldable[A, F[_] : Foldable]($: F[A]) {
    def doForEach(f: A => Unit): F[A] = {
      // because scalaz isn't tail recursive 🔔 shame 🔔 shame 🔔
      var list: List[A] = Nil
      implicitly[Foldable[F]].any($) { e => list = e :: list; false }
      list foreach f
      $
    }
    def printPerLine(): F[A] = doForEach(println)
  }
}

