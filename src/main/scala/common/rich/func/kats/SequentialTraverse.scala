package common.rich.func.kats

import cats.Monad
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherId}
import cats.syntax.functor.toFunctorOps

import scala.collection.mutable.ArrayBuffer

import common.rich.func.kats.ToMoreFunctorOps.toMoreFunctorOps

trait SequentialTraverse[F[_]] {
  /**
   * Unlike [[cats.Traverse.traverse]], doesn't apply the function in parallel (i.e., using
   * [[cats.Applicative.map2]]), but sequentially. This can be useful if you want to maintain order
   * for some "misbehaving" effects, like [[scala.concurrent.Future]].
   */
  def mapM[G[_]: Monad, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}
object SequentialTraverse {
  implicit object ListSequentialTraverse extends SequentialTraverse[List] {
    override def mapM[G[_]: Monad, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      Monad[G].tailRecM[(List[A], List[B]), List[B]]((fa, Nil: List[B])) { case (current, result) =>
        current match {
          case Nil => result.reverse.asRight.pure
          case head :: next => f(head).map(res => Left(next, res :: result))
        }
      }
  }

  implicit object VectorSequentialTraverse extends SequentialTraverse[Vector] {
    override def mapM[G[_]: Monad, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      vectorAux[G, A, B, Vector](fa)(f)
  }

  implicit object SeqSequentialTraverse extends SequentialTraverse[Seq] {
    override def mapM[G[_]: Monad, A, B](fa: Seq[A])(f: A => G[B]): G[Seq[B]] =
      vectorAux[G, A, B, Seq](fa.toVector)(f)
  }

  private def vectorAux[G[_]: Monad, A, B, CC[_] <: Seq[_]](
      fa: Vector[A],
  )(f: A => G[B]): G[CC[B]] = {
    val buffer = new ArrayBuffer[B](fa.length)
    Monad[G]
      .tailRecM[Int, CC[B]](0)(i =>
        if (i == fa.length)(buffer.toVector.asInstanceOf[CC[B]]).asRight.pure
        else f(fa(i)).listen(buffer.+=).as(Left(i + 1)),
      )
  }
}
