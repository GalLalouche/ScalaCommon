package common.rich.func.kats

import cats.{Alternative, Applicative, CoflatMap, Eval, Foldable, Functor, Monad, Monoid, MonoidK, StackSafeMonad, Traverse, TraverseFilter}
import cats.data.Chain

import scala.annotation.tailrec
import scala.collection.compat.IterableOnce
import scala.collection.mutable

import common.rich.func.kats.PlainSeqInstances.{traverseDirectly, traverseFilterDirectly, traverseVoidDirectly}

import common.rich.RichT.{lazyT, richT}
import common.rich.primitives.RichBoolean.richBoolean

/**
 * [[cats.instances.seq.catsStdInstancesForSeq]] are for [[collection.immutable.Seq]], not the
 * regular [[Seq]] because https://typelevel.org/cats/faq.html#where-are-implicit-instances-for-seq.
 *
 * Note, importing `PlainSeqInstances.plainSeqInstances` will often get flagged as "unused" by
 * IntelliJ, optimized away, and then the compiler will complain it can't find the [[Seq]] instance
 * 🤦. Either use an `implicit val`, extend this `trait`, or mark the import as always used in the
 * IDE settings.
 *
 * Literally just copy-pasted their implementation, except where noted.
 */
trait PlainSeqInstances {
  implicit object plainSeqInstances
      extends Traverse[Seq]
      with Monad[Seq]
      with Alternative[Seq]
      with CoflatMap[Seq] {

    def empty[A]: Seq[A] = Seq.empty[A]

    def combineK[A](x: Seq[A], y: Seq[A]): Seq[A] = x ++ y

    override def combineAllOptionK[A](as: IterableOnce[Seq[A]]): Option[Seq[A]] = {
      val iter = as.toIterator
      if (iter.hasNext.isFalse)
        return None
      // Original used list builder. Fuck that.
      val builder = Vector.newBuilder[A]
      while (iter.hasNext)
        builder ++= iter.next()
      Some(builder.result())
    }

    override def fromIterableOnce[A](as: IterableOnce[A]): Seq[A] = {
      val builder = Seq.newBuilder[A]
      builder ++= as
      builder.result()
    }

    override def prependK[A](a: A, fa: Seq[A]): Seq[A] = a +: fa

    override def appendK[A](fa: Seq[A], a: A): Seq[A] = fa :+ a

    def pure[A](x: A): Seq[A] = Seq(x)

    override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] =
      fa.map(f)

    def flatMap[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] =
      fa.flatMap(f)

    override def map2[A, B, Z](fa: Seq[A], fb: Seq[B])(f: (A, B) => Z): Seq[Z] =
      if (fb.isEmpty) Seq.empty // do O(1) work if either is empty
      else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

    private[this] val evalEmpty: Eval[Seq[Nothing]] = Eval.now(Seq.empty)

    override def map2Eval[A, B, Z](fa: Seq[A], fb: Eval[Seq[B]])(f: (A, B) => Z): Eval[Seq[Z]] =
      if (fa.isEmpty) evalEmpty // no need to evaluate fb
      else fb.map(fb => map2(fa, fb)(f))

    def coflatMap[A, B](fa: Seq[A])(f: Seq[A] => B): Seq[B] = {
      @tailrec def loop(builder: mutable.Builder[B, Seq[B]], as: Seq[A]): Seq[B] =
        as match {
          case _ +: rest => loop(builder += f(as), rest)
          case _ => builder.result()
        }
      loop(Seq.newBuilder[B], fa)
    }

    def foldLeft[A, B](fa: Seq[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    def foldRight[A, B](fa: Seq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable.iterateRight(fa, lb)(f)

    override def foldMap[A, B](fa: Seq[A])(f: A => B)(implicit B: Monoid[B]): B =
      B.combineAll(fa.iterator.map(f))

    def tailRecM[A, B](a: A)(fn: A => Seq[Either[A, B]]): Seq[B] = {
      val buf = Seq.newBuilder[B]
      var state = List(fn(a).iterator)
      @tailrec
      def loop(): Unit =
        state match {
          case Nil => ()
          case h :: tail if h.isEmpty =>
            state = tail
            loop()
          case h :: tail =>
            h.next() match {
              case Right(b) =>
                buf += b
                loop()
              case Left(a) =>
                state = (fn(a).iterator) :: h :: tail
                loop()
            }
        }
      loop()
      buf.result()
    }

    override def size[A](fa: Seq[A]): Long = fa.size.toLong

    override def get[A](fa: Seq[A])(idx: Long): Option[A] =
      if (idx < Int.MaxValue && fa.size > idx && idx >= 0) Some(fa(idx.toInt)) else None

    override def foldMapK[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit G: MonoidK[G]): G[B] = {
      def loop(i: Int): Eval[G[B]] =
        if (i < fa.length) G.combineKEval(f(fa(i)), Eval.defer(loop(i + 1)))
        else Eval.now(G.empty)
      loop(0).value
    }

    final override def traverse[G[_], A, B](
        fa: Seq[A],
    )(f: A => G[B])(implicit G: Applicative[G]): G[Seq[B]] =
      G match {
        case x: StackSafeMonad[G] =>
          // Original used toList. Fuck that.
          x.map(traverseDirectly(fa)(f)(x))(_.toVector)
        case _ =>
          G.map(Chain.traverseViaChain(fa.toIndexedSeq)(f))(_.toVector)
      }

    override def traverseVoid[G[_], A, B](
        fa: Seq[A],
    )(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = G match {
      case x: StackSafeMonad[G] => traverseVoidDirectly(fa)(f)(x)
      // Spartanized.
      case _ => foldRight(fa, Eval.now(G.unit))((a, acc) => G.map2Eval(f(a), acc)(().const2)).value
    }

    override def mapWithIndex[A, B](fa: Seq[A])(f: (A, Int) => B): Seq[B] =
      fa.zipWithIndex.map(ai => f(ai._1, ai._2))

    override def zipWithIndex[A](fa: Seq[A]): Seq[(A, Int)] =
      fa.zipWithIndex

    override def exists[A](fa: Seq[A])(p: A => Boolean): Boolean =
      fa.exists(p)

    override def isEmpty[A](fa: Seq[A]): Boolean = fa.isEmpty

    override def foldM[G[_], A, B](fa: Seq[A], z: B)(
        f: (B, A) => G[B],
    )(implicit G: Monad[G]): G[B] = {
      val length = fa.length
      G.tailRecM((z, 0)) { case (b, i) =>
        if (i < length) G.map(f(b, fa(i)))(b => Left((b, i + 1)))
        else G.pure(Right(b))
      }
    }

    override def fold[A](fa: Seq[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

    override def toList[A](fa: Seq[A]): List[A] = fa.toList

    override def toIterable[A](fa: Seq[A]): Iterable[A] = fa

    override def reduceLeftOption[A](fa: Seq[A])(f: (A, A) => A): Option[A] =
      fa.reduceLeftOption(f)

    override def find[A](fa: Seq[A])(f: A => Boolean): Option[A] = fa.find(f)

    override def algebra[A]: Monoid[Seq[A]] = seqMonoid

    def functor: Functor[Seq] = this

    override def collectFirst[A, B](fa: Seq[A])(pf: PartialFunction[A, B]): Option[B] =
      fa.collectFirst(pf)

    override def collectFirstSome[A, B](fa: Seq[A])(f: A => Option[B]): Option[B] =
      fa.collectFirst(Function.unlift(f))
  }

  implicit object traverseSeq extends TraverseFilter[Seq] {
    override def traverse: Traverse[Seq] = plainSeqInstances
    override def mapFilter[A, B](fa: Seq[A])(f: (A) => Option[B]): Seq[B] =
      fa.collect(Function.unlift(f))

    override def filter[A](fa: Seq[A])(f: (A) => Boolean): Seq[A] = fa.filter(f)

    override def filterNot[A](fa: Seq[A])(f: A => Boolean): Seq[A] = fa.filterNot(f)

    override def collect[A, B](fa: Seq[A])(f: PartialFunction[A, B]): Seq[B] = fa.collect(f)

    override def flattenOption[A](fa: Seq[Option[A]]): Seq[A] = fa.flatten

    def traverseFilter[G[_], A, B](
        fa: Seq[A],
    )(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[Seq[B]] =
      G match {
        case x: StackSafeMonad[G] =>
          x.map(traverseFilterDirectly(fa)(f)(x))(_.toVector)
        case _ =>
          G.map(Chain.traverseFilterViaChain(fa.toIndexedSeq)(f))(_.toVector)
      }

    override def filterA[G[_], A](
        fa: Seq[A],
    )(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[Seq[A]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(Seq.empty[A])))((x, xse) =>
          G.map2Eval(f(x), xse)((b, Seq) => if (b) x +: Seq else Seq),
        )
        .value
  }

  implicit def seqMonoid[A]: Monoid[Seq[A]] =
    PlainSeqInstances.seqMonoid.asInstanceOf[Monoid[Seq[A]]]
}

object PlainSeqInstances extends PlainSeqInstances {
  // Copy-pasted from a package private implementation and Spartanized.
  private def traverseDirectly[G[_], A, B](
      fa: IterableOnce[A],
  )(f: A => G[B])(implicit G: StackSafeMonad[G]): G[Chain[B]] =
    fa.foldLeft(G.pure(Chain.empty[B])) { case (accG, a) => G.map2(accG, f(a))(_ :+ _) }

  private def traverseVoidDirectly[G[_], A, B](
      fa: IterableOnce[A],
  )(f: A => G[B])(implicit G: StackSafeMonad[G]): G[Unit] = {
    val iter = fa.toIterator
    if (iter.hasNext) {
      val first = iter.next()
      G.void(iter.foldLeft(f(first))((g, a) => G.productR(g)(f(a))))
    } else
      G.unit
  }

  private def traverseFilterDirectly[G[_], A, B](
      fa: IterableOnce[A],
  )(f: A => G[Option[B]])(implicit G: StackSafeMonad[G]): G[Chain[B]] =
    fa.foldLeft(G.pure(Chain.empty[B]))((bldrG, a) => G.map2(bldrG, f(a))(_.joinOption(_)(_ :+ _)))

  private val seqMonoid: Monoid[Seq[Any]] = new Monoid[Seq[Any]] {
    override def empty: Seq[Any] = Seq.empty
    override def combine(x: Seq[Any], y: Seq[Any]): Seq[Any] = x ++ y
  }
}
