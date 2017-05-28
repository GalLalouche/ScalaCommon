package common.rich.path

import java.io.File
import java.nio.file.{FileAlreadyExistsException, Files}

import common.rich.path.RichPath.poorPath

/** Helper class for Directory methods */
class Directory(val dir: File) extends RichPath(dir) {
  require(dir.getAbsoluteFile isDirectory, s"${dir.getAbsolutePath} is not a directory")

  /**
   * Adds a new file under the directory, if one doesn't exist
   * @param name The file's name
   * @return the file created
   */
  def addFile(name: String): RichFile = {
    val $ = new File(dir, name)
    $.createNewFile()
    $
  }
  /**
   * Adds a new sub-directory under this directory, if one doesn't exist
   * @param name The directory's name
   * @return the directory created
   */
  def addSubDir(name: String) = {
    val $ = new File(dir, name)
    $.mkdir()
    Directory($)
  }

  /**
   * @return all direct sub directory of this directory
   */
  def dirs: Seq[Directory] =
    Option(dir.listFiles)
        .getOrElse(Array())
        .toVector
        .filter(_.isDirectory)
        .map(Directory(_))

  /**
   * All direct files of this directory, that are *not* directories
   */
  def files: Seq[File] =
    Option(dir.listFiles)
        .getOrElse(Array())
        .toVector
        .filterNot(_.isDirectory)

  def paths: Seq[RichPath] = files.map(RichFile.apply) ++ dirs
  /**
   * Deletes all files and directories in this dir recursively including itself
   */
  def deleteAll() {
    System.gc()
    def deleteAll(d: Directory) {
      System.gc()
      d.dirs.foreach(deleteAll)
      d.files.foreach(x => {if (x.exists && x.delete == false) println("could not delete: " + x)})
      if (d.dir.exists && d.dir.delete == false) {
        System.gc()
        println("could not delete: " + d.dir)
      }
    }
    deleteAll(this)
  }
  /**
   * Deletes all files and directories in this dir recursively <b>not</b> including itself
   */
  def clear() {
    deleteAll()
    dir.mkdir
  }
  /**
   * @return all files that are not dirs nested inside this dir (in any given depth)
   */
  def deepFiles: Stream[File] = files.toStream ++ dirs.flatMap(_.deepFiles)
  def deepDirs: Stream[Directory] = dirs.toStream ++ dirs.flatMap(_.deepDirs)
  def deepPaths: Stream[RichPath] = paths.toStream ++ dirs.flatMap(_.deepPaths)

  /**
   * Clones the directory, creating a copy of it suffixed with "clone" by default.
   * This will override any other cloned directory
   */
  def cloneDir(suffix: String = "_clone"): Directory = {
    require(suffix.nonEmpty, "Must provide a suffix when cloning")
    val newName = name + suffix
    parent.addSubDir(newName).deleteAll() // delete previous directory if it exists
    copyTo(parent, newName)
  }

  def copyTo(parentDir: Directory): Directory = copyTo(parentDir, name)
  def copyTo(parentDir: Directory, newName: String): Directory = {
    val destinationFile = parentDir \ newName
    if (destinationFile.exists)
      throw new FileAlreadyExistsException(destinationFile.getPath)
    val $ = parent.addSubDir(newName)
    def copyFile(f: File, cloneDir: File): Unit = {
      val outputFile = new File(cloneDir, f.getName)
      if (f.isDirectory) {
        outputFile.mkdir()
        for (f <- f.listFiles)
          copyFile(f, outputFile)
      } else
        Files.copy(f.toPath, outputFile.toPath)
    }
    this.listFiles.foreach(copyFile(_, $))
    $
  }
  //
  //	def copyTo(newFile: File): RichFile = {
}

object Directory {
  def apply(f: File): Directory = new Directory(f)
  def apply(s: String): Directory = Directory(new File(s))
  def apply(p: RichPath): Directory = Directory(p.p)
  /** Creates all directories along the path as needed */
  def makeDir(f: File): Directory = {
    if (false == (f.exists && f.isDirectory))
      require(f mkdirs, "Could not create directories path " + f)
    Directory(f)
  }

  def makeDir(fullPath: String): Directory = makeDir(new File(fullPath))
}
