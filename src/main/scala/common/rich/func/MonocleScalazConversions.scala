package common.rich.func

import scalaz.Isomorphism.IsoSet

trait MonocleScalazConversions {
  implicit class MonocleScalazIsoConversions[A, B](iso: monocle.Iso[A, B]) {
    def toScalaz: IsoSet[A, B] = IsoSet(iso.get, iso.reverseGet)
  }
}
object MonocleScalazConversions extends MonocleScalazConversions
