package common.path.ref

import java.io.File
import java.time.LocalDateTime

import common.rich.RichT.richT

/**
 * [[PathRef]]s have several useful properties over plain old [[File]]s.
 *
 *   - Existence is checked at creation time.
 *   - There are separate and mutually exclusive types for files and directories.
 *   - Paths are always canonical, and equals and hashCode are based on said path, so they are
 *     faster, and hashCode can be cached.
 */
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
  override def toString: String = s"${this.simpleName}($path)"
  /** Throws on root, i.e., has if [[hasParent]] is `false`. */
  def parent: S#D
  def parents: Seq[S#D]
  def hasParent: Boolean
  /** This is only here for ease of use, but ''should'' have been in [[DirectoryRef]] instead. */
  def /(path: String): PathRef
  def creationTime: LocalDateTime
  def lastModifiedTime: LocalDateTime
  // While this *starts out* as true, it may change if the underlying file is deleted.
  def exists: Boolean
}
