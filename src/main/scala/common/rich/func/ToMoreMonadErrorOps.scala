package common.rich.func

import scala.language.higherKinds

import scalaz.{-\/, \/, \/-, MonadError}
import scalaz.std.option.optionInstance

import common.rich.RichT._

object ToMoreMonadErrorOps {
  import scalaz.syntax.monadError._
  import common.rich.func.ToMoreFoldableOps._

  class FilteredException(str: String) extends NoSuchElementException(str)
  implicit class toMoreMonadErrorOps[F[_], A, S]($: F[A])(implicit F: MonadError[F, S]) {
    def handleErrorFlat(f: S => A): F[A] = $.handleError(f andThen (F.pure(_)))
    def orElse(other: => A): F[A] = orElseTry(F pure other)
    def orElseTry(other: => F[A]): F[A] = $ handleError other.const
    def handleButKeepOriginal(other: S => F[A]): F[A] = $ handleError other orElseTry $
    def filterWith(p: A => Boolean, error: => S): F[A] = filterWithF(p, error.const)
    def filterWithF(p: A => Boolean, error: A => S): F[A] =
      mapEither(e => if (p(e)) \/-(e) else -\/(error(e)))
    def mapError(f: S => S): F[A] = $ handleError f.andThen(F.raiseError)
    def listenError(f: S => Any): F[A] = mapError(_ applyAndReturn f)
    def filterMap(f: A => Option[S]): F[A] = mapEither(e => f(e).mapHeadOrElse(-\/.apply, \/-(e)))
    def mapEither[B](f: A => S \/ B): F[B] = for {
      e <- $
      result <- f(e).fold(F.raiseError, F.pure(_))
    } yield result
    def foldEither[B](f: S \/ A => B): F[B] = foldEitherF(f.andThen(F.pure(_)))
    def foldEitherF[B](f: S \/ A => F[B]): F[B] = $.flatMap(e => f(\/-(e))).handleError(e => f(-\/(e)))
    def mFilter(p: A => F[Boolean], error: A => S): F[A] = for {
      e <- $
      predValue <- p(e)
      result <- if (predValue) F pure e else F raiseError error(e)
    } yield result
  }
  implicit class toMoreMonadErrorThrowableOps[F[_], A]($: F[A])(implicit F: MonadError[F, Throwable]) {
    def filterEquals(other: => A): F[A] =
      filterWithMessageF(_ == other, e =>
        s"Expected: <$other>,\n" +
            s"but was:  <$e>")
    def filterMapMessage(f: A => Option[String]): F[A] =
      mapEitherMessage(e => f(e).mapHeadOrElse(-\/.apply, \/-(e)))
    def filterWithMessage(p: A => Boolean, message: String): F[A] = $.filterWithMessageF(p, message.const)
    def filterWithMessageF(p: A => Boolean, message: A => String): F[A] =
      $.mapEitherMessage(e => if (p(e)) \/-(e) else -\/(message(e)))
    def mapEitherMessage[B](f: A => String \/ B): F[B] = for {
      e <- $
      result <- f(e).fold(e => F.raiseError(new FilteredException(e)), F.pure(_))
    } yield result
    def filterWithStacktrace(p: A => Boolean, message: String = "Failed filter"): F[A] =
      filterWithStacktraceF(p, message.const)
    def filterWithStacktraceF(p: A => Boolean, message: A => String): F[A] = {
      val stackTrace = Thread.currentThread.getStackTrace drop 3
      def error(a: A): F[A] = {
        val ex = new FilteredException(message(a))
        ex setStackTrace stackTrace
        F raiseError ex
      }
      $.flatMap(e => if (p(e)) $ else error(e))
    }
  }
}
