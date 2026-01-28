package common.rich.path.ref.io

import java.io.File

import better.files.{File => BFile, FileExtensions}

import common.rich.path.ref.PathRef

trait IOPath extends PathRef { self: File =>
  override def getAbsolutePath: String = path
  override def getCanonicalPath: String = path
  private[io] def witness: PackageWitness
  override type S = IOSystem
  def better: BFile = self.toScala
  override def name: String = getName
  override def path: String = getPath
  override def parent: IODirectory
}
