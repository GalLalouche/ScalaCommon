package common.test.memory_ref

import java.nio.file.attribute.BasicFileAttributes
import java.time.LocalDateTime
import java.util.concurrent.ConcurrentHashMap

import cats.implicits.toFunctorOps
import rx.lang.scala.Observable

import common.rich.func.kats.ObservableInstances.observableInstances

import common.path.ref.DirectoryRef
import common.rich.ConvertersVersionSpecific
import common.rx.RichObservable.richObservable

sealed abstract class MemoryDir(val path: String) extends DirectoryRef with MemoryPath {
  private val filesByName =
    ConvertersVersionSpecific.toScala(new ConcurrentHashMap[String, MemoryFile]())
  private val dirsByName =
    ConvertersVersionSpecific.toScala(new ConcurrentHashMap[String, MemoryDir]())

  override def getFile(name: String) = filesByName.get(name)
  override def addFile(name: String) = getFile(name).getOrElse {
    val $ = MemoryFile(this, name)
    filesByName += name -> $
    $
  }
  override def getDir(name: String): Option[MemoryDir] = dirsByName.get(name)
  override def addSubDir(name: String) = addSubDir(name, LocalDateTime.now())
  def addSubDir(name: String, lastModified: LocalDateTime): MemoryDir = getDir(name).getOrElse {
    val $ = SubDir(this, name, lastModified)
    dirsByName += name -> $
    $
  }
  override def dirs: Iterator[MemoryDir] = dirsByName.values.toSeq.sortBy(_.name).iterator
  override def files: Iterator[MemoryFile] = filesByName.values.toSeq.sortBy(_.name).iterator

  def deleteFile(name: String): Boolean = {
    val hasFile = filesByName.contains(name)
    if (hasFile)
      filesByName -= name
    hasFile
  }
  override def clear(): MemoryDir = {
    filesByName.clear()
    dirsByName.values.foreach(_.clear())
    dirsByName.clear()
    this
  }
  def deepDirs: Iterator[S#D] = deepDirsObservable.map(_._1).toVectorBlocking.iterator
  def containsFileWithExtension(extensions: Iterable[String]): Boolean =
    files.exists(extensions exists _.hasExtension)
  def deepDirsObservable: Observable[(S#D, BasicFileAttributes)] =
    Observable.from(dirsByName.values).fproduct(_.basicFileAttributes).flatMap { d =>
      Observable.just(d) ++ d._1.deepDirsObservable
    }
  def deepFilesObservable: Observable[(S#F, BasicFileAttributes)] =
    Observable.from(filesByName.values).fproduct(_.basicFileAttributes) ++
      Observable.from(dirsByName.values).flatMap(_.deepFilesObservable)
  def deepFiles: Iterator[S#F] = files ++ dirs.flatMap(_.deepFiles)
  override def getSubPath(name: String): Option[MemoryPath] =
    filesByName.get(name).orElse(dirsByName.get(name))
  private[memory_ref] override def packageWitness: PackageWitness = PackageWitness
}
private case class SubDir(
    override val parent: MemoryDir,
    override val name: String,
    override val lastModifiedTime: LocalDateTime = LocalDateTime.now(),
    override val creationTime: LocalDateTime = LocalDateTime.now(),
) extends MemoryDir(parent.path + "/" + name) {
  override def hasParent = true
}
class MemoryRoot extends MemoryDir("/") {
  override def exists: Boolean = true
  override def name: String = "/"
  override def parent = throw new UnsupportedOperationException("MemoryRoot has no parent")
  override def hasParent = false
  override val path = s"root(${System.identityHashCode(this)})/"
  override val lastModifiedTime: LocalDateTime = LocalDateTime.now()
  override val creationTime: LocalDateTime = LocalDateTime.now()
}
