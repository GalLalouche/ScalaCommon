package common.rich.func.kats

import cats.{Eval, Foldable, Monad, MonoidK}

import scala.annotation.tailrec
import scala.collection.{IndexedSeq => ImmutableIndexedSeq}

/** Useful instances since enumeratum uses [[IndexedSeq]] explicitly. */
object IndexedSeqInstances {
  implicit object ImmutableImmutableIndexedSeqEv
      extends Monad[ImmutableIndexedSeq]
      with MonoidK[ImmutableIndexedSeq]
      with Foldable[ImmutableIndexedSeq] {
    override def empty[A]: ImmutableIndexedSeq[A] = IndexedSeq.empty[A]
    override def foldLeft[A, B](fa: ImmutableIndexedSeq[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)
    override def foldRight[A, B](fa: ImmutableIndexedSeq[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B],
    ): Eval[B] = {
      // Copy-pasted from the Vector implementation
      val length = fa.length
      def loop(i: Int): Eval[B] = if (i < length) f(fa(i), Eval.defer(loop(i + 1))) else lb
      Eval.defer(loop(0))
    }
    override def pure[A](x: A): ImmutableIndexedSeq[A] = IndexedSeq(x)
    override def flatMap[A, B](fa: ImmutableIndexedSeq[A])(
        f: A => ImmutableIndexedSeq[B],
    ): ImmutableIndexedSeq[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(
        f: A => ImmutableIndexedSeq[Either[A, B]],
    ): ImmutableIndexedSeq[B] = {
      // Copy-pasted from the Vector implementation
      val buf = Vector.newBuilder[B]
      var state = List(f(a).iterator)
      @tailrec
      def loop(): Unit =
        state match {
          case Nil => ()
          case h :: tail if h.isEmpty =>
            state = tail
            loop()
          case h :: tail =>
            h.next() match {
              case Right(b) => buf += b
              case Left(a) => state = f(a).iterator :: h :: tail
            }
            loop()
        }
      loop()
      buf.result()
    }
    override def combineK[A](
        x: ImmutableIndexedSeq[A],
        y: ImmutableIndexedSeq[A],
    ): ImmutableIndexedSeq[A] = x ++ y
  }
}
