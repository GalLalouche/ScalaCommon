package common.rich

import java.io.{ByteArrayOutputStream, File, FileOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets

import scala.annotation.tailrec
import scala.io.Source

import common.rich.RichT._

object RichInputStream {
  implicit class richInputStream(private val $ : InputStream) extends AnyVal {
    /**
     * Reads a line from the input stream (not including the line endings), or [[None]] if EOF is
     * reached. Does '''not''' consume the remainder of the stream. Will return `Some("")` for empty
     * lines (e.g., `"foo\n\n"` contains two lines, the second being empty).
     */
    def readLine(): Option[String] = {
      // 🤮🤮🤮🤮🤮
      // Can't use BufferedReader or Source since they would consume more of the stream than just the line.
      val baos = new ByteArrayOutputStream(1 << 7)
      def readUntilNewLine(): Boolean = {
        var b = 0
        var read = false
        b = $.read()
        while (b != -1) {
          read = true
          if (b == '\n')
            return true
          if (b == '\r') {
            $.read().ensuring(_ == '\n', "Expected \\n after \\r")
            return true
          }
          baos.write(b)
          b = $.read()
        }
        read
      }
      if (readUntilNewLine()) Some(baos.toString(StandardCharsets.UTF_8)) else None
    }
    /** Closes the input stream. */
    def asString(): String = {
      // Not using Using for source backwards compatibility with Scala 2.12
      val source = Source.fromInputStream($)
      try source.mkString
      finally source.close()
    }
    /** Closes the input stream. */
    def toBytes(): Array[Byte] = new ByteArrayOutputStream().<|(writeTo).toByteArray
    /** Closes the input stream. */
    def writeTo(outputStream: OutputStream): Unit = try {
      val buffer = new Array[Byte](1 << 14)
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
    } finally $.close()
    /** Closes the input stream. */
    def writeTo(f: File): Unit = {
      val fos = new FileOutputStream(f)
      try writeTo(fos)
      finally fos.close()
    }
  }
}
