package common.rich.func

import common.rich.RichT._

import scala.language.higherKinds
import scalaz.MonadError
import scalaz.std.OptionInstances
import scalaz.syntax.{ToMonadErrorOps, ToTraverseOps}

trait ToMoreMonadErrorOps extends ToMonadErrorOps with ToTraverseOps {
  class FilteredException(str: String) extends NoSuchElementException(str)
  implicit class toMoreMonadErrorOps[F[_], A, S]($: F[A])(implicit F: MonadError[F, S]) {
    def orElse(other: => A): F[A] = orElseTry(F.pure(other))
    def orElseTry(other: => F[A]): F[A] = $ handleError other.const
    def handleButKeepOriginal(other: S => F[A]): F[A] = $ handleError other orElseTry $
    def filterWith(p: A => Boolean, error: => S): F[A] = filterWithF(p, error.const)
    def filterWithF(p: A => Boolean, error: A => S): F[A] = mFilter(p andThen (F.pure(_)), error)
    def mFilter(p: A => F[Boolean], error: A => S): F[A] = for {
      e <- $
      predValue <- p(e)
      result <- if (predValue) F pure e else F raiseError error(e)
    } yield result
    def mapError(f: S => S): F[A] = $ handleError f.andThen(F.raiseError)
  }
  implicit class toMoreMonadErrorOptionalOps[F[_], A, S]($: F[Option[A]])(
      implicit ev: MonadError[F, S]) extends ToMoreFoldableOps with OptionInstances {
    def ifNone(other: => A): F[A] = ifNoneTry(ev pure other)
    def ifNoneTry(other: => F[A]): F[A] = $.flatMap(_.mapHeadOrElse(ev.pure(_), other))
  }
  implicit class toMoreMonadErrorThrowableOps[F[_], A]($: F[A])(
      implicit ev: MonadError[F, Throwable]) {
    def filterWithMessage(p: A => Boolean, message: String = "Failed filter"): F[A] =
      $.filterWithMessageF(p, message.const)
    def filterWithMessageF(p: A => Boolean, message: A => String): F[A] =
      $.filterWithF(p, e => new FilteredException(message(e)))
    def filterWithStacktrace(p: A => Boolean, message: String = "Failed filter"): F[A] =
      filterWithStacktraceF(p, message.const)
    def filterWithStacktraceF(p: A => Boolean, message: A => String): F[A] = {
      val stackTrace = Thread.currentThread.getStackTrace drop 3
      def error(a: A): F[A] = {
        val ex = new FilteredException(message(a))
        ex.setStackTrace(stackTrace)
        ev raiseError ex
      }
      $.flatMap(e => if (p(e)) $ else error(e))
    }
  }
}
