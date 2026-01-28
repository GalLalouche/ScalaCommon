package common.rich.path.ref

import java.io.File

import common.rich.RichT._

trait RefSystem { self =>
  type S <: RefSystem
  type P <: PathRef { type S = self.S }
  type F <: FileRef { type S = self.S }
  type D <: DirectoryRef { type S = self.S }
}
object RefSystem {
  type Aux[S0] = RefSystem { type S = S0 }
}

/** Either a file or a directory. */
trait PathRef {
  type S <: RefSystem
  /** Returns the canonical path. */
  def path: String
  final override lazy val hashCode: Int = path.hashCode
  final override def equals(obj: Any): Boolean = obj match {
    case that: PathRef => this.path == that.path
    case _ => false
  }
  final def normalizedPath: String = path.replace(File.separatorChar, '/')
  def name: String
  override def toString: String = s"${this.simpleName}: $path"
  /** Throws on root, i.e., has if [[hasParent]] is `false`. */
  def parent: S#D
  def parents: Seq[S#D]
  def hasParent: Boolean
  def /(path: String): PathRef with File
}

trait PathRefFactory {
  def parsePath(path: String): PathRef
  def parseFilePath(path: String): FileRef
  def parseDirPath(path: String): DirectoryRef
}
