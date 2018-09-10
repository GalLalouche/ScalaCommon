package common.rich.func

import monocle.PLens

object TuplePLenses {
  def tuple2First[A, B, C]: PLens[(A, B), (C, B), A, C] = PLens[(A, B), (C, B), A, C](_._1)(c => c -> _._2)
  def tuple2Second[A, B, C]: PLens[(A, B), (A, C), B, C] = PLens[(A, B), (A, C), B, C](_._2)(c => _._1 -> c)
}
