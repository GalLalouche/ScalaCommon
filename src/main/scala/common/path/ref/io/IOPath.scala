package common.path.ref.io

import java.io.File
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.time.{LocalDateTime, ZoneId}

import better.files.{File => BFile, FileExtensions}

import common.path.ref.PathRef

trait IOPath extends PathRef { self: File =>
  override def getAbsolutePath: String = path
  override def getCanonicalPath: String = path
  private[io] def witness: PackageWitness
  override type S = IOSystem
  def better: BFile = self.toScala
  override def name: String = getName
  override def path: String = getPath
  override def parent: IODirectory
  override def /(path: String): IOPath = {
    val file = new File(self, path)
    if (file.isDirectory) IODirectory(file) else IOFile(file)
  }
  override def creationTime: LocalDateTime = fromFileTime(_.creationTime)
  override def lastModifiedTime: LocalDateTime = fromFileTime(_.lastModifiedTime)
  def lastAccessTime: LocalDateTime = fromFileTime(_.lastAccessTime)
  private def fromFileTime(f: BasicFileAttributes => FileTime): LocalDateTime =
    LocalDateTime.ofInstant(
      f(Files.readAttributes(toPath, classOf[BasicFileAttributes])).toInstant,
      ZoneId.systemDefault(),
    )
  @inline final def asFile: File = self
}

object IOPath {
  def fromFile(file: File): IOPath =
    if (file.isDirectory) IODirectory(file) else IOFile(file)
}
