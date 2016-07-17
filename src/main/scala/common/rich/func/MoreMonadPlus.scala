package common.rich.func

import scalaz.MonadPlus

object MoreMonadPlus {
  implicit object SeqMonadPlus extends MonadPlus[Seq] {
    override def bind[A, B](fa: Seq[A])(f: (A) => Seq[B]): Seq[B] = fa flatMap f
    override def plus[A](a: Seq[A], b: => Seq[A]): Seq[A] = a ++ b
    override def point[A](a: => A): Seq[A] = Seq(a)
    override def empty[A]: Seq[A] = Seq()
  }
}
