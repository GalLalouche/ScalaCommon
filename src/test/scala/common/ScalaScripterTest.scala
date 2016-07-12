//package common
//
//import java.io.File
//import common.rich.path.RichFile._
//import org.scalatest._
//
//class ScalaScripterTest extends FreeSpec with DirectorySpecs with AuxSpecs {
//  def processToString(f: File)(implicit scripter: File => File): String = {
//    val is = scripter(f).execute.getInputStream
//    val output = scala.io.Source.fromInputStream(is).getLines().toList
//    assert(output.size == 1)
//    output.head
//  }
//
//  "In Windows" - {
//    "Simple" - {
//      implicit val conforms: File => File = ScalaScripter.forWindows(_: File, tempDir.addFile("foobar.bat"))
//      "hello world" in {
//        tempFile write """println("Hello, World!")"""
//        processToString(tempFile) shouldReturn "Hello, World!"
//      }
//
//      "Slightly more complex" in {
//        tempFile write
//          """
//            |val x = 1 + 1
//            |println(x * 2)
//          """.stripMargin
//        processToString(tempFile) shouldReturn "4"
//      }
//
//      "Using classpaths" - {
//        val f = new File("D:\\tmp\\intellij output\\artifacts\\SoftwareMetrics.jar")
//        implicit val conforms: File => File = ScalaScripter
//          .withClassPath(f)
//          .forWindows(_: File, tempDir.addFile("foobar.bat"))
//        "using richfile" in {
//          val readmeFile = tempDir.addFile("readme.txt")
//          readmeFile write "Hi!"
//          tempFile write
//            s"""
//              import java.io.File
//              import common.rich.path.RichFile._
//              println(new File(\"\"\"${readmeFile.getAbsolutePath}\"\"\").readAll)
//            """.stripMargin
//
//          processToString(tempFile) shouldReturn "Hi!"
//        }
//      }
//    }
//  }
//
//}
