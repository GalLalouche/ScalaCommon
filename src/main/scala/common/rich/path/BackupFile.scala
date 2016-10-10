package common.rich.path

import java.io.File
import java.nio.file._

import common.rich.path.RichFile._

/** An abstraction for backing up and restoring files. By default, these files are deleted on exit regardless of their use. */
class BackupFile(file: File, deleteOnExit: Boolean = true) {
  require(file.exists)
  require(file.isDirectory == false)
  private val backupFile = {
    val $ = new File(file.getParentFile, file.getName + ".bak")
    $.createNewFile
    if (deleteOnExit)
      $.deleteOnExit()
    Files.copy(file.toPath, $.toPath, StandardCopyOption.REPLACE_EXISTING)
    $
  }
  assert(backupFile.exists)
  assert(backupFile != file)
  assert(file hasSameContentAs backupFile)

  /** overrides any changes made to the backup file with the copy backed up by this file */
  def restore() {
    Files.copy(backupFile.toPath, file.toPath, StandardCopyOption.REPLACE_EXISTING)
    assert(file hasSameContentAs backupFile)
  }
}
