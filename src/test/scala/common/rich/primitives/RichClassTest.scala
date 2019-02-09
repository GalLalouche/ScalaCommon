package common.rich.primitives

import common.AuxSpecs
import common.rich.primitives.RichClass._
import org.scalatest.FreeSpec

class RichClassTest extends FreeSpec with AuxSpecs {
  "richClass" - {
    "isAssignableTo" - {
      class A
      class B extends A
      "Same" in {classOf[A].isAssignableTo(classOf[A]) shouldReturn true}
      "Extends" in {classOf[B].isAssignableTo(classOf[A]) shouldReturn true}
      "Super fails" in {classOf[A].isAssignableTo(classOf[B]) shouldReturn false}
    }
    
  }
}
