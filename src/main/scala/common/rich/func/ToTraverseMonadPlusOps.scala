package common.rich.func

import scala.language.higherKinds

import scalaz.{Applicative, MonadPlus, State, Traverse}
import scalaz.Free.Trampoline
import scalaz.syntax.functor.ToFunctorOps
import scalaz.syntax.monadPlus.ToMonadPlusOps
import scalaz.syntax.traverse.ToTraverseOps

import common.rich.primitives.RichBoolean._

trait ToTraverseMonadPlusOps {
  implicit class toTraverseMonadPlusOps[F[_]: MonadPlus: Traverse, A]($ : F[A]) {
    // Scalaz only provides functions for List and Vector for some reason.
    def filterM[G[_]: Applicative](p: A => G[Boolean]): G[F[A]] =
      $.traverse(ToMoreFunctorOps.toProduct(p)).map(_.filter(_._2).tmap(_._1))
    def uniqueBy[B](f: A => B): F[A] = $.filterM { a =>
      // Stack-safe solution adapted from https://stackoverflow.com/a/30676115/736508
      val b = f(a)
      for {
        existing <- State.get[Set[B]].lift[Trampoline]
        _ <- State.modify[Set[B]](_ + b).lift[Trampoline]
      } yield existing(b).isFalse
    }.eval(Set()).run
  }
}
object ToTraverseMonadPlusOps extends ToTraverseMonadPlusOps
