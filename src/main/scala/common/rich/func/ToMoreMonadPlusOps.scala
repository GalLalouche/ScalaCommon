package common.rich.func

import scala.collection.mutable
import scala.language.higherKinds
import scala.util.Try

import scalaz.MonadPlus
import scalaz.syntax.functor.ToFunctorOps
import scalaz.syntax.monadPlus.ToMonadPlusOps

import common.rich.RichT._

trait ToMoreMonadPlusOps {
  implicit class toMoreMonadPlusOps[A, F[_] : MonadPlus]($: F[A]) {
    def filterNot(f: A => Boolean): F[A] = $.filter(f.andThen(!_))
    def tryMap[B](f: A => B): F[B] =
      $.map(e => Try(f(e)))
          .filter(_.isSuccess)
          .map(_.get)
    // because flatMap only works on collections
    def oMap[B](f: A => Option[B]): F[B] = $.map(f).filter(_.isDefined).map(_.get)
    def present[B](implicit ev: A <:< Option[B]): F[B] = oMap(ev.apply)
    def select[B <: A : Manifest]: F[B] = oMap(_.safeCast[B])
    /** Not thread-safe! */
    def uniqueBy[B](f: A => B): F[A] = {
      val set = new mutable.HashSet[B]()
      $.filter(a => {
        val b = f(a)
        if (set(b))
          false
        else {
          set += b
          true
        }
      })
    }
  }
}

object ToMoreMonadPlusOps extends ToMoreMonadPlusOps
