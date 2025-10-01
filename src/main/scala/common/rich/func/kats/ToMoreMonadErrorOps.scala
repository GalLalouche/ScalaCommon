package common.rich.func.kats

import cats.{ApplicativeError, MonadError}
import cats.implicits.{catsSyntaxApplicativeError, toFlatMapOps}
import cats.syntax.functor.toFunctorOps

import common.rich.func.kats.ToMoreFoldableOps.toMoreFoldableOps
import common.rich.func.scalazz.ToMoreMonadErrorOps.FilteredException

import common.rich.RichT._

trait ToMoreMonadErrorOps {
  implicit class toMoreApplicativeErrorOps[F[_], A, S]($ : F[A])(implicit
      F: ApplicativeError[F, S],
  ) {
    def orElseFlat(other: => A): F[A] = $.orElse(F.pure(other))
    def handleButKeepOriginal(other: S => F[A]): F[A] = $.handleErrorWith(other).orElse($)
    def mapError(f: S => S): F[A] = $.handleErrorWith(f.andThen(F.raiseError))
    def listenError(f: S => Any): F[A] = mapError(_.applyAndReturn(f))
  }
  implicit class toMoreMonadErrorOps[F[_], A, S]($ : F[A])(implicit F: MonadError[F, S]) {
    def filterWith(p: A => Boolean, error: => S): F[A] = filterWithF(p, error.const)
    def filterWithF(p: A => Boolean, error: A => S): F[A] =
      mapEither(e => if (p(e)) Right(e) else Left(error(e)))
    def collectHandle(f: PartialFunction[S, A]): F[A] = collectHandleF(f andThen (F.pure(_)))
    def collectHandleF(f: PartialFunction[S, F[A]]): F[A] = foldEitherF {
      case Left(a) => f.lift(a).getOrElse(F.raiseError(a))
      case Right(b) => F.pure(b)
    }
    def filterMap(f: A => Option[S]): F[A] =
      mapEither(e => f(e).mapHeadOrElse(Left.apply, Right(e)))
    def mapEither[B](f: A => Either[S, B]): F[B] = for {
      e <- $
      result <- f(e).fold(F.raiseError, F.pure)
    } yield result
    def foldEither[B](f: Either[S, A] => B): F[B] = foldEitherF(f andThen F.pure)
    def foldEitherF[B](f: Either[S, A] => F[B]): F[B] =
      $.flatMap(e => f(Right(e))).handleErrorWith(e => f(Left(e)))
    def mFilter(p: A => F[Boolean], error: A => S): F[A] = for {
      e <- $
      predValue <- p(e)
      result <- if (predValue) F.pure(e) else F.raiseError(error(e))
    } yield result
  }

  implicit class toMoreMonadErrorThrowableOps[F[_], A]($ : F[A])(implicit
      F: MonadError[F, Throwable],
  ) {
    def filterEquals(other: => A): F[A] =
      filterWithMessageF(
        _ == other,
        e =>
          s"Expected: <$other>,\n" +
            s"but was:  <$e>",
      )
    def filterMapMessage(f: A => Option[String]): F[A] =
      mapEitherMessage(e => f(e).mapHeadOrElse(Left.apply, Right(e)))
    def filterWithMessage(p: A => Boolean, message: String): F[A] =
      $.filterWithMessageF(p, message.const)
    def filterWithMessageF(p: A => Boolean, message: A => String): F[A] =
      $.mapEitherMessage(e => if (p(e)) Right(e) else Left(message(e)))
    def mapEitherMessage[B](f: A => Either[String, B]): F[B] = for {
      e <- $
      result <- f(e).fold(e => F.raiseError(new FilteredException(e)), F.pure)
    } yield result
    def filterWithStacktrace(p: A => Boolean, message: String = "Failed filter"): F[A] =
      filterWithStacktraceF(p, message.const)
    def filterWithStacktraceF(p: A => Boolean, message: A => String): F[A] = {
      val stackTrace = Thread.currentThread.getStackTrace.drop(3)
      def error(a: A): F[A] = {
        val ex = new FilteredException(message(a))
        ex.setStackTrace(stackTrace)
        F.raiseError(ex)
      }
      $.flatMap(e => if (p(e)) $ else error(e))
    }
  }
}

object ToMoreMonadErrorOps extends ToMoreMonadErrorOps {
  def guard[F[_], S](b: Boolean, s: => S)(implicit F: ApplicativeError[F, S]): F[Unit] =
    if (b) F.pure(()) else F.raiseError(s)
  def guardMessage[F[_]](b: Boolean, s: => String)(implicit
      F: ApplicativeError[F, Throwable],
  ): F[Unit] = if (b) F.pure(()) else F.raiseError(new Exception(s))
  class FilteredException(str: String) extends NoSuchElementException(str)
}
