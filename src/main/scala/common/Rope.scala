package common

import common.rich.RichT._
import common.rich.RichTuple._

import scala.annotation.tailrec
import scala.collection._
import scala.collection.generic._

/**
 * Allows adding items at the end and start of the list in O(1),
 * As well as converting from a sequence in O(1).
 * The true power of the rope is in its ability to be concatenated in O(1) with itself and other sequences.
 * Although this increases by O(1) all subsequent queries on the rope,
 */
sealed abstract class Rope[+T]
  extends Traversable[T]
          with Seq[T]
          with SeqLike[T, Rope[T]]
          with TraversableLike[T, Rope[T]]
          with GenericTraversableTemplate[T, Rope] {
  override def companion: GenericCompanion[Rope] = Rope

  val length: Int
  def apply(index: Int): T
  override def isEmpty = length == 0
  def ::[B >: T](x: B): Rope[B] = this match {
    case EmptyNode => Leaf(x)
    case _ => InnerVertex(Leaf(x), this)
  }

  def +[B >: T](x: B): Rope[B] = this match {
    case EmptyNode => Leaf(x)
    case _ => InnerVertex(this, Leaf(x))
  }

  def ++[B >: T](other: Rope[B]): Rope[B] =
    if (this.isEmpty) other
    else if (other.isEmpty) this
    else InnerVertex(this, other)

  override def splitAt(index: Int): (Rope[T], Rope[T]) = this match {
    case InnerVertex(downSon, rightSon) =>
      if (index > downSon.length)
        rightSon.splitAt(index - downSon.length).mapTo(e => (downSon ++ e._1, e._2))
      else
        downSon.splitAt(index).mapTo(e => (e._1, e._2 ++ rightSon))
    case ListVertex(data) => data.splitAt(index).map(_.to[Rope])
    case _ => super.splitAt(index).map(_.to[Rope])
  }

  override def iterator = super[TraversableLike].toBuffer.toIterator
}

case object EmptyNode extends Rope {
  override val length: Int = 0
  override def head = throw new NoSuchElementException
  override def apply(index: Int) = throw new IndexOutOfBoundsException
  override def equals(a: Any) = a.getClass == this.getClass
  override def foreach[U](f: Nothing => U) {}
}

case class Leaf[+T](e: T) extends Rope[T] {

  override val length: Int = 1
  override val head = e
  override def apply(index: Int): T = if (index == 0) e else throw new IndexOutOfBoundsException
  override def foreach[U](f: T => U) { f(e) }
}

private case class InnerVertex[T](downSon: Rope[T], rightSon: Rope[T]) extends Rope[T] {
  assert(downSon.nonEmpty)
  assert(rightSon.nonEmpty)

  override val length: Int = downSon.length + rightSon.length

  override def apply(index: Int): T =
    if (index < downSon.length)
      downSon.apply(index)
    else
      rightSon.apply(index - downSon.length)

  override val head = downSon.head
  override def foreach[U](f: T => U) {
    @tailrec
    def eatAllSimple(xs: List[Rope[T]]): (Option[InnerVertex[T]], List[Rope[T]]) = xs match {
      case Nil => (None, Nil)
      case x :: tail => x match {
        case e: InnerVertex[T] => (Some(e), tail)
        case _ =>
          x foreach f
          eatAllSimple(tail)
      }
    }
    @tailrec
    def aux(current: InnerVertex[T], remainder: List[Rope[T]]) {
      current.downSon match {
        case ds: InnerVertex[T] => aux(ds, current.rightSon :: remainder)
        case _ =>
          current.downSon foreach f
          val (nextCurrent, newRemainder) = eatAllSimple(current.rightSon :: remainder)
          if (nextCurrent.isEmpty)
            return
          aux(nextCurrent.get, newRemainder)
      }
    }
    aux(this, List())
  }
}

private case class ListVertex[T](data: Seq[T]) extends Rope[T] {
  assert(data.lengthCompare(1) > 0)

  override lazy val length: Int = data.length

  override def apply(index: Int): T = data apply index

  override val head = data.head

  override def foreach[U](f: T => U) { data foreach f }
}

object Rope extends TraversableFactory[Rope] {

  def newBuilder[T]: mutable.Builder[T, Rope[T]] = new mutable.Builder[T, Rope[T]] {
    var $: Rope[T] = EmptyNode

    override def +=(elem: T) = {
      $ += elem
      this
    }

    override def ++=(xs: TraversableOnce[T]) = {
      xs match {
        case ts: Rope[T] => $ ++= ts
        case _ =>
          val v = xs.toSeq
          if (v.length > 1)
            $ ++= ListVertex(v)
          else
            super.++=(v)
      }
      this
    }


    def clear() {
      $ = EmptyNode
    }

    def result() = $
  }

  implicit def canBuildFrom[T]: CanBuildFrom[Rope[_], T, Rope[T]] =
    new CanBuildFrom[Rope[_], T, Rope[T]] {
      def apply(from: Rope[_]) = newBuilder[T]

      def apply() = newBuilder[T]
    }
}
