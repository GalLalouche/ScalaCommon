package common.rich.func.scalazz

import org.scalatest.{FreeSpec, Matchers}

class PointTest extends FreeSpec with Matchers {
  "Future is a Point without an implicit ExecutionContext" in {
    """
      |import scala.concurrent.Future
      |import common.rich.func.scalazz.BetterFutureInstances._
      |Point[Future].point(4)""".stripMargin should compile
  }
  "No implicit problems with Point instance when scalaz instance is imported (scalaz)" in {
    """
      |import scala.concurrent.Future
      |import scalaz.std.scalaFuture.futureInstance
      |import scala.concurrent.ExecutionContext
      |implicit val ec: ExecutionContext = ???
      |Point[Future].point(4)""".stripMargin should compile
  }
  "No implicit problems with Point instance when scalaz instance is imported (better)" in {
    """
      |import scala.concurrent.Future
      |import common.rich.func.scalazz.BetterFutureInstances._
      |import scala.concurrent.ExecutionContext
      |implicit val ec: ExecutionContext = ???
      |Point[Future].point(4)""".stripMargin should compile
  }
}
