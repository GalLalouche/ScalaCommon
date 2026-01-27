package common.rich.path

import java.io.File

import common.rich.path.RichFile.richFile

/** A special RichFile that supports appending lines in different levels of indentations. */
class IndentableRichFile private (f: File, indentLevel: Int) {
  require(indentLevel >= 0, "indentLevel cannot be negative")
  def this(f: File) = this(f, 0)

  def increaseIndent = new IndentableRichFile(f, indentLevel + 1)
  def decreaseIndent = new IndentableRichFile(f, indentLevel - 1)
  def resetIndentation() = new IndentableRichFile(f, 0)
  def clear(): IndentableRichFile = {
    f.clear()
    this.resetIndentation()
  }
  def appendLine(s: String): this.type = {
    f.appendLine(("\t" * indentLevel) + s)
    this
  }
}
