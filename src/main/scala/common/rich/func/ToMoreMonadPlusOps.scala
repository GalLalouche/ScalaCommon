package common.rich.func

import scala.collection.mutable
import scala.language.higherKinds
import scala.util.Try

import scalaz.MonadPlus

import common.rich.RichT._

object ToMoreMonadPlusOps {
  import scalaz.syntax.monadPlus._

  implicit class toMoreMonadPlusOps[A, F[_] : MonadPlus]($: F[A]) {
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
