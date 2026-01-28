package common.test.memory_ref

import org.scalatest.OneInstancePerTest
import org.scalatest.freespec.AnyFreeSpec

import common.test.AuxSpecs

class MemoryRefFactoryTest extends AnyFreeSpec with AuxSpecs with OneInstancePerTest {
  private val root = new MemoryRoot
  private val $ = new MemoryRefFactory(root)
  "root" in {
    $.parseDirPath(root.path) shouldReturn root
  }
  "Top level file" in {
    val file = root.addFile("foo")
    $.parseFilePath(file.path) shouldReturn file
  }
  "Nested file" in {
    val file = root.addSubDir("foo").addSubDir("bar").addFile("moo")
    $.parseFilePath(file.path) shouldReturn file
  }
  "Top level dir" in {
    val dir = root.addSubDir("foo")
    $.parseDirPath(dir.path) shouldReturn dir
  }
  "Nested dir" in {
    val dir = root.addSubDir("foo").addSubDir("bar").addSubDir("moo")
    $.parseDirPath(dir.path) shouldReturn dir
  }
}
