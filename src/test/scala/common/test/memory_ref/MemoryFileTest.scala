package common.test.memory_ref

import org.scalatest.OneInstancePerTest
import org.scalatest.freespec.AnyFreeSpec

import common.rich.RichT.richT
import common.test.AuxSpecs

class MemoryFileTest extends AnyFreeSpec with AuxSpecs with OneInstancePerTest {
  private val root = new MemoryRoot
  private val file = root.addFile("test.txt")

  "outputStream" - {
    "single byte write should be reflected in content" in {
      file.outputStream.<|(_.write('x')).<|(_.close())
      file.readAll shouldReturn "x"
    }
    "should truncate existing content on open" in {
      file.write("existing content")
      file.outputStream.<|(_.write("new".getBytes)).<|(_.write("bytes".getBytes)).<|(_.close())
      file.readAll shouldReturn "newbytes"
    }
  }
}
