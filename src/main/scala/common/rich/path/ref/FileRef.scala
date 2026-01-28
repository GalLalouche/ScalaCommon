package common.rich.path.ref

import java.io.File

/**
 * Differences between this and [[File]]:
 *   - Must exist on disk at the time of object creation.
 *   - Its path is always canonical, so we can cache [[hashCode]] and [[equals]] is faster.
 */
trait FileRef extends PathRef { self: File =>
  type S <: RefSystem

  override def parents = parent +: parent.parents.asInstanceOf[Seq[S#D]]
  override def hasParent = true
}
