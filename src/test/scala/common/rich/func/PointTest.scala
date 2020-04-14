package common.rich.func

import org.scalatest.{FreeSpec, Matchers}

class PointTest extends FreeSpec with Matchers {
  "Future is a Point without an implicit ExecutionContext" in {
    """
      |import scala.concurrent.Future
      |Point[Future].point(4)""".stripMargin should compile
  }
  "No implicit problems with Point instance when scalaz instance is imported" in {
    """
      |import scala.concurrent.Future
      |import scalaz.std.scalaFuture.futureInstance
      |import scala.concurrent.ExecutionContext
      |implicit val ec: ExecutionContext = ???
      |Point[Future].point(4)""".stripMargin should compile
  }
}
