package common.rich

import java.io.{ByteArrayOutputStream, File, FileOutputStream, InputStream, OutputStream}

import scala.annotation.tailrec
import scala.io.Source
import RichT._

object RichInputStream {
  implicit class richInputStream(private val $: InputStream) extends AnyVal {
    def asString: String = Source.fromInputStream($).mkString
    def toBytes: Array[Byte] = new ByteArrayOutputStream().<|(writeTo).toByteArray
    def writeTo(outputStream: OutputStream): Unit = {
      val buffer = new Array[Byte](16384)
      @tailrec
      def go(): Unit = {
        val nRead = $.read(buffer, 0, buffer.length)
        if (nRead == -1)
          return
        outputStream.write(buffer, 0, nRead)
        go()
      }
      go()
      outputStream.flush()
    }
    def writeTo(f: File): Unit = {
      val fos = new FileOutputStream(f)
      try writeTo(fos)
      finally fos.close()
    }
  }
}
