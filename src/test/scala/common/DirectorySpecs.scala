package common

import common.rich.path.TempDirectory
import org.scalatest.{BeforeAndAfter, Suite}

/**
 * Several helping methods and fixtures for testing classes that works with IO
 */
trait DirectorySpecs extends AuxSpecs with BeforeAndAfter {
  self: Suite =>
  after { tempDir.clear }
  val tempDir = TempDirectory()
  lazy val tempFile = tempDir.addFile("tempFile")
}
