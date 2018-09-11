package common.rich.func

import common.rich.RichT._

import scala.language.higherKinds

import scalaz.{-\/, \/, \/-, MonadError}
import scalaz.std.OptionInstances
import scalaz.syntax.{ToMonadErrorOps, ToTraverseOps}

trait ToMoreMonadErrorOps extends ToMonadErrorOps with ToTraverseOps {
  class FilteredException(str: String) extends NoSuchElementException(str)
  implicit class toMoreMonadErrorOps[F[_], A, S]($: F[A])(implicit F: MonadError[F, S])
      extends ToMoreFoldableOps with OptionInstances {
    def handleErrorFlat(f: S => A): F[A] = $.handleError(f andThen (F.pure(_)))
    def orElse(other: => A): F[A] = orElseTry(F pure other)
    def orElseTry(other: => F[A]): F[A] = $ handleError other.const
    def handleButKeepOriginal(other: S => F[A]): F[A] = $ handleError other orElseTry $
    def filterWith(p: A => Boolean, error: => S): F[A] = filterWithF(p, error.const)
    def filterWithF(p: A => Boolean, error: A => S): F[A] = \/(e => if (p(e)) \/-(e) else -\/(error(e)))
    def mapError(f: S => S): F[A] = $ handleError f.andThen(F.raiseError)
    def listenError(f: S => Any): F[A] = mapError(_ applyAndReturn f)
    def filterMap(f: A => Option[S]): F[A] = \/(e => f(e).mapHeadOrElse(-\/.apply, \/-(e)))
    def \/(f: A => S \/ A): F[A] = for {
      e <- $
      result <- f(e).fold(F.raiseError, F.pure(_))
    } yield result
    def either(f: A => Either[S, A]): F[A] = \/(f.andThen(scalaz.\/.fromEither))
    def mFilter(p: A => F[Boolean], error: A => S): F[A] = for {
      e <- $
      predValue <- p(e)
      result <- if (predValue) F pure e else F raiseError error(e)
    } yield result
  }
  implicit class toMoreMonadErrorThrowableOps[F[_], A]($: F[A])(
      implicit F: MonadError[F, Throwable])
      extends ToMoreFoldableOps with OptionInstances {
    def filterEquals(other: => A): F[A] =
      filterWithMessageF(_ == other, e => s"Expected <$other>, but was <$e>")
    def filterWithMessage(p: A => Boolean, message: String = "Failed filter"): F[A] =
      $.filterWithMessageF(p, message.const)
    def filterMapMessage(f: A => Option[String]): F[A] = for {
      e <- $
      result <- f(e).mapHeadOrElse(filterWithMessage(false.const, _), $)
    } yield result
    def filterWithMessageF(p: A => Boolean, message: A => String): F[A] =
      $.filterWithF(p, e => new FilteredException(message(e)))
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
