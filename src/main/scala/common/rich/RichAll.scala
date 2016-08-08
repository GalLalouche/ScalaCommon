package common.rich

import java.io.File

import common.rich.collections._
import common.rich.path.RichFile
import common.rich.path.RichFile._
import common.rich.primitives._

import scala.reflect.ClassTag

/** Convenience object for all rich implicits. */
object RichAll {
  implicit def richArray[T]($: Array[Array[T]]) = RichArray.richArray($)
  implicit def richSeqArray[T]($: Seq[Seq[T]])(implicit m: ClassTag[T]) = RichArray.richSeqArray($)
  implicit def richDouble[T]($: Double) = RichDouble.Rich($)
  implicit def richInt[T]($: Int) = RichInt.Rich($)
  implicit def richBoolean[T]($: Boolean) = RichBoolean.richBoolean($)
  implicit def richSeq[T]($: Seq[T]) = RichSeq.richSeq($)
  implicit def richIterator[T]($: Iterator[T]) = RichIterator.Rich($)
  implicit def richSeqTuples2[T, U]($: Seq[(T, U)]) = RichSeq.richSeqTuplesDouble($)
  implicit def richSeqTuples3[T, U, S]($: Seq[(T, U, S)]) = RichSeq.richSeqTuplesTriplets($)
  implicit def richSeqTuples4[T, U, S, W]($: Seq[(T, U, S, W)]) = RichSeq.richSeqTuplesQuadruplets($)
  implicit def richString($: String) = RichString.richString($)
  implicit def richT[T]($: T) = RichT.richT($)
  implicit def richTraversableDouble($: Traversable[Double]) = RichTraversableDouble.richTraversableDouble($)
  implicit def richTraversableInt($: Traversable[Int]) = RichTraversableDouble.richInt($)
  implicit def richTraversableOnce($: TraversableOnce[Int]) = RichTraversableOnce.richTraversableOnce($)
  implicit def richTuple[T]($: (T, T)) = RichTuple.richTuple($)
  implicit def richTupleSeqs[T, S]($: (Seq[T], Seq[S])) = RichTuple.richTupleSeqs($)
  implicit def richSet[T]($: Set[T]) = RichSet.Rich($)
  implicit def richOption[T]($: Option[T]) = RichOption.Rich($)
  implicit def richVector[T]($: Vector[Double]) = RichVector.Rich($)
  implicit def richFile($: File) = RichFile.richFile($)
  implicit def poorFile($: RichFile) = RichFile.richFile($)
}
