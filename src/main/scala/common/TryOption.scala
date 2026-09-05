package common

import cats.{Eval, Foldable, Monad}

import scala.annotation.tailrec
import scala.util.Try

import common.rich.func.kats.ToMoreFoldableOps.toMoreFoldableOps

import common.Trither.{TriLeft, TriMid, TriRight}
import common.TryOption.{Failure, NoValue}

/**
 * Used when something can be missing, but we also want to capture any errors. For example, parsing
 * a sub-object from a JSON: maybe the key isn't there (in which case it would be [[NoValue]]), or
 * maybe there's an error during parsing (in which case it would be [[Failure]]).
 */
sealed trait TryOption[+A] {
  def get: A
  def toOption: Option[A]
  def run: Option[Try[A]]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](default: => TryOption[B]): TryOption[B]
  def withFilter(p: A => Boolean): TryOption[A]
  def map[B](f: A => B): TryOption[B]
  def flatMap[B](f: A => TryOption[B]): TryOption[B]
  def flatMapF[B](f: A => Option[B])(implicit dummy: DummyImplicit): TryOption[B]
  def flatMapF[B](f: A => Try[B])(implicit d1: DummyImplicit, d2: DummyImplicit): TryOption[B]
  /** The Unit stands for NoValue */
  def toTrither: Trither[Throwable, Unit, A]
}
object TryOption {
  case class Success[A](value: A) extends TryOption[A] {
    override def get: A = value
    override def toOption: Option[A] = Some(value)
    override def getOrElse[B >: A](default: => B): B = value
    override def orElse[B >: A](default: => TryOption[B]): TryOption[B] = this
    override def withFilter(p: A => Boolean): TryOption[A] = if (p(value)) this else NoValue
    override def map[B](f: A => B): TryOption[B] = Success(f(value))
    override def flatMap[B](f: A => TryOption[B]): TryOption[B] = f(value)
    override def flatMapF[B](f: A => Option[B])(implicit dummy: DummyImplicit): TryOption[B] =
      fromOption(f(value))
    override def flatMapF[B](
        f: A => Try[B],
    )(implicit d1: DummyImplicit, d2: DummyImplicit): TryOption[B] =
      fromTry(f(value))
    override def run: Option[Try[A]] = Some(scala.util.Success(value))
    override def toTrither: Trither[Nothing, Nothing, A] = TriRight(value)
  }
  case object NoValue extends TryOption[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("No value present")
    override def toOption: Option[Nothing] = None
    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](default: => TryOption[B]): TryOption[B] = default
    override def withFilter(p: Nothing => Boolean): TryOption[Nothing] = this
    override def map[B](f: Nothing => B): TryOption[B] = this
    override def flatMap[B](f: Nothing => TryOption[B]): TryOption[B] = this
    override def flatMapF[B](f: Nothing => Option[B])(implicit dummy: DummyImplicit): TryOption[B] =
      this
    override def flatMapF[B](
        f: Nothing => Try[B],
    )(implicit d1: DummyImplicit, d2: DummyImplicit): TryOption[B] = this
    override def run: Option[Try[Nothing]] = None
    override def toTrither: Trither[Nothing, Unit, Nothing] = TriMid(())
  }
  case class Failure(exception: Throwable) extends TryOption[Nothing] {
    override def get: Nothing = throw exception
    override def toOption: Option[Nothing] = None
    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](default: => TryOption[B]): TryOption[B] = default
    override def withFilter(p: Nothing => Boolean): TryOption[Nothing] = this
    override def map[B](f: Nothing => B): TryOption[B] = this
    override def flatMap[B](f: Nothing => TryOption[B]): TryOption[B] = this
    override def flatMapF[B](f: Nothing => Option[B])(implicit dummy: DummyImplicit): TryOption[B] =
      this
    override def flatMapF[B](
        f: Nothing => Try[B],
    )(implicit d1: DummyImplicit, d2: DummyImplicit): TryOption[B] = this
    override def run: Option[Try[Nothing]] = Some(scala.util.Failure(exception))
    override def toTrither: Trither[Throwable, Nothing, Nothing] = TriLeft(exception)
  }

  def fromOption[A](opt: Option[A]): TryOption[A] = opt.mapHeadOrElse(Success(_), NoValue)
  def fromTry[A](t: Try[A]): TryOption[A] = t.fold(Failure(_), Success(_))

  implicit object Instances extends Monad[TryOption] with Foldable[TryOption] {
    override def pure[A](x: A): TryOption[A] = Success(x)
    override def flatMap[A, B](fa: TryOption[A])(f: A => TryOption[B]): TryOption[B] = fa.flatMap(f)
    override def map[A, B](fa: TryOption[A])(f: A => B): TryOption[B] = fa.map(f)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => TryOption[Either[A, B]]): TryOption[B] = f(a) match {
      case Success(Left(nextA)) => tailRecM(nextA)(f)
      case Success(Right(b)) => Success(b)
      case f: Failure => f
      case NoValue => NoValue
    }
    override def foldLeft[A, B](fa: TryOption[A], b: B)(f: (B, A) => B): B =
      fa.toOption.foldLeft(b)(f)
    override def foldRight[A, B](fa: TryOption[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B],
    ): Eval[B] = fa.toOption.foldRight(lb)(f)
  }
}
