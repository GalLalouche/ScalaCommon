package common.rich.func

import scala.language.{higherKinds, reflectiveCalls}

import scalaz.{~>, Applicative, Functor, OptionT}

object RichOptionT {
  implicit class richOptionT[F[_], A](private val $: OptionT[F, A]) extends AnyVal {
    def subFlatMap[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
      OptionT(F.map($.run)(_ flatMap f))
  }

  // I.e., Option[A] ~> OptionT[F, A]
  def app[F[_] : Applicative]: Option ~> ({type λ[α] = OptionT[F, α]})#λ =
    new (Option ~> ({type λ[α] = OptionT[F, α]})#λ) {
      override def apply[A](fa: Option[A]) = OptionT(Applicative[F].point(fa))
    }
}
