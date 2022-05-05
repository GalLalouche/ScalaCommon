package common

object Orderings {
  // Since Ordering.by(_.foo).reversed breaks type inference.
  def byReversed[A, B: Ordering](f: A => B): Ordering[A] = Ordering.by[A, B](f).reverse

  def fromSeqCheck[A](xs: Seq[A]): Ordering[A] = fromSeqPartial(xs, NoneOrder.NoNones)
  def fromSeqPartial[A](xs: Seq[A], noneOrder: NoneOrder): Ordering[A] = {
    val indices = xs.view.zipWithIndex.toMap
    (x, y) =>
      (indices.get(x), indices.get(y)) match {
        case (None, None) =>
          if (noneOrder == NoneOrder.NoNones) throw new NoSuchElementException(x.toString) else 0
        case (None, _) => noneOrder match {
          case NoneOrder.First => -1
          case NoneOrder.Last => 1
          case NoneOrder.NoNones => throw new NoSuchElementException(x.toString)
        }
        case (_, None) => noneOrder match {
          case NoneOrder.First => 1
          case NoneOrder.Last => -1
          case NoneOrder.NoNones => throw new NoSuchElementException(y.toString)
        }
        case (Some(xIndex), Some(yIndex)) => xIndex.compareTo(yIndex)
      }
  }
  sealed trait NoneOrder
  object NoneOrder {
    case object First extends NoneOrder
    case object Last extends NoneOrder
    // Internal usage only, clients should use fromSeqCheck.
    private[Orderings] case object NoNones extends NoneOrder
  }

  implicit class richOrdering[A](private val $: Ordering[A]) extends AnyVal {
    def noneLast: Ordering[Option[A]] = {
      case (None, None) => 0
      case (None, _) => 1
      case (_, None) => -1
      case (Some(x), Some(y)) => $.compare(x, y)
    }
    // Explicit is bette than implicit
    def noneFirst: Ordering[Option[A]] = Ordering.Option($)
  }
}
