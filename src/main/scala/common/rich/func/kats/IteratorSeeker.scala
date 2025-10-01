package common.rich.func.kats

import java.util

import scala.annotation.tailrec

/** Helper for implementing tailRecM for [[Iterator]]. */
private class IteratorSeeker[A, B](
    initial: A,
    f: A => Iterator[Either[A, B]],
) extends Iterator[B] {
  private val linkedList: util.LinkedList[Iterator[Either[A, B]]] =
    new util.LinkedList[Iterator[Either[A, B]]]()
  linkedList.add(f(initial))
  private var head: Option[B] = None
  @tailrec private def fetchNextHead(): Option[B] = {
    val first = linkedList.peek()
    if (first == null)
      return None
    if (first.hasNext)
      first.next() match {
        case Left(value) =>
          val nextIterable = f(value)
          linkedList.addFirst(nextIterable)
          fetchNextHead()
        case Right(value) => Some(value)
      }
    else {
      linkedList.pop()
      fetchNextHead()
    }
  }
  private def updateNextHead(): Unit = head = fetchNextHead()
  updateNextHead()

  override def hasNext: Boolean = head.isDefined
  override def next(): B = {
    val $ = head.get
    updateNextHead()
    $
  }
}
