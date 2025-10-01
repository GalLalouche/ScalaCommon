package common.rich.func.kats

import cats.{Apply, Eval}
import cats.syntax.apply.catsSyntaxApplyOps

trait ToMoreApplyOps {
  implicit class toMoreApplyOps[F[_]: Apply, A]($ : F[A]) {
    def <<*[B](fb: => F[B]): F[A] = $.map2Eval(Eval.later(fb))((a, _) => a).value
    def *>>[B](fb: => F[B]): F[B] = $.map2Eval(Eval.later(fb))((_, b) => b).value
    def tuple[B](fb: => F[B]): F[(A, B)] = $.map2(fb)((x, y) => (x, y))
  }
}

object ToMoreApplyOps extends ToMoreApplyOps
