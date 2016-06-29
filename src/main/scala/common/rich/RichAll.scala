package common.rich

import java.io.File

import common.rich.collections._
import common.rich.path.RichFile
import common.rich.path.RichFile._
import common.rich.primitives._

import scala.reflect.ClassTag

/**
 * Convenience object for all rich implicits
 */
object RichAll {
  implicit def richArray[T]($: Array[Array[T]]) = RichArray.richArray($)
  implicit def richSeqArray[T]($: Seq[Seq[T]])(implicit m: ClassTag[T]) = RichArray.richSeqArray($)
  implicit def richDouble[T]($: Double) = RichDouble.richDouble($)
  implicit def richInt[T]($: Int) = RichInt.richInt($)
  implicit def richSeq[T]($: Seq[T]): RichSeq[T] = RichSeq.richSeq($)
  implicit def richIterator[T]($: Iterator[T]) = RichIterator.richIterator($)
  implicit def richSeqTuples2[T, U]($: Seq[(T, U)]) = RichSeq.richSeqTuplesDouble($)
  implicit def richSeqTuples3[T, U, S]($: Seq[(T, U, S)]) = RichSeq.richSeqTuplesTriplets($)
  implicit def richSeqTuples4[T, U, S, W]($: Seq[(T, U, S, W)]) = RichSeq.richSeqTuplesQuadruplets($)
  implicit def richString($: String) = RichString.richString($)
  implicit def richT[T]($: T) = RichT.richT($)
  implicit def richTraversable[T]($: Traversable[T]) = RichTraversable.richTraversable($)
  implicit def richTraversableOption[T]($: Traversable[Option[T]]) = RichTraversable.richTraversableOption($)
  implicit def richTraversableDouble($: Traversable[Double]) = RichTraversableDouble.richTraversableDouble($)
  implicit def richTraversableInt($: Traversable[Int]) = RichTraversableDouble.richTraversableInt($)
  implicit def richTuple[T]($: (T, T)) = RichTuple.richTuple($)
  implicit def richTupleSeqs[T, S]($: (Seq[T], Seq[S])) = RichTuple.richTupleSeqs($)
  implicit def richSet[T]($: Set[T]) = RichSet.richSet($)
  implicit def richOption[T]($: Option[T]) = RichOption.richOption($)
  implicit def richVector[T]($: Vector[Double]) = RichVector.rich($)

  implicit def richFile($: File) = RichFile.richFile($)
  implicit def poorFile($: RichFile) = RichFile.richFile($)
}
