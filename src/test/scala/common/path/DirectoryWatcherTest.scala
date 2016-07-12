package common.path

import java.io.{File, PrintWriter}

import common.{AuxSpecs, DirectorySpecs}
import mains.programs.DirectoryWatcher
import mains.programs.DirectoryWatcher._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FreeSpec, OneInstancePerTest}
import rx.lang.scala.Observer

import scala.collection.mutable

class DirectoryWatcherTest extends FreeSpec with AuxSpecs with DirectorySpecs with MockitoSugar with OneInstancePerTest {

	val observer = new Observer[DirectoryWatchEvent] {
		var fileEvents = mutable.Queue[DirectoryWatchEvent]()
		override def onNext(value: DirectoryWatchEvent) {
			synchronized {
				fileEvents += value
				notify()
			}
		}
		override def onError(error: Throwable) { super.onError(error) }
		override def onCompleted() { super.onCompleted() }
		def isEmpty = synchronized {fileEvents.isEmpty}
		def waitForAnswer: DirectoryWatchEvent = {
			synchronized {
				if (fileEvents.nonEmpty)
					return fileEvents.dequeue()
				else
					wait(100)
				if (fileEvents.isEmpty)
					sys.error("Found no file event...")
				fileEvents.dequeue()
			}
		}
	}

	"DirectoryWatcher should" - {
		val $ = DirectoryWatcher.apply(tempDir)
		$ subscribe observer
		"handle new creations" - {
			"new file created" in {
				val f = tempDir addFile "foobar.txt"
				observer.waitForAnswer shouldReturn FileCreated(f)
			}
			"new folder created" in {
				tempDir addSubDir "foo"
				observer.waitForAnswer shouldReturn DirectoryCreated(tempDir / "foo" /)
			}
			"new folder created in recursive" in {
				val foo = tempDir addSubDir "foo"
				observer.waitForAnswer shouldReturn DirectoryCreated(tempDir / "foo" /)
				foo addSubDir "bar"
				observer.waitForAnswer shouldReturn DirectoryCreated(tempDir / "foo" / "bar" /)
			}
		}

		"handle deletes" - {
			"file deleted" in {
				val file = tempDir addFile "foo.bar"
				observer.waitForAnswer
				file.delete
				observer.waitForAnswer shouldReturn FileDeleted(file)
			}
			"directory deleted" in {
				val d = tempDir addSubDir "foo"
				observer.waitForAnswer
				d.deleteAll
				observer.waitForAnswer shouldReturn FileDeleted(d)
			}
		}

		"handle renames" - {
			"directory renamed" in {
				val d = tempDir addSubDir "foo"
				observer.waitForAnswer
				d.dir.renameTo(new File(d.parent, "bar"))
				observer.waitForAnswer shouldReturn FileDeleted(tempDir \ "foo")
				observer.waitForAnswer shouldReturn DirectoryCreated(tempDir / "bar" /)
			}
			"file renamed" in {
				val f = tempDir addFile "foo"
				observer.waitForAnswer
				f.renameTo(new File(f.parent, "bar"))
				observer.waitForAnswer shouldReturn FileDeleted(tempDir \ "foo")
				observer.waitForAnswer shouldReturn FileCreated(tempDir \ "bar")
			}
		}

		"Handle file writes" - {
			def write(f: File) {
				new PrintWriter(f).append('a')
			}
			"return file modified" in {
				val f = tempDir addFile "foo.txt"
				observer.waitForAnswer
				write(f)
				observer.waitForAnswer shouldReturn FileModified(f)
			}

			"even if file existed before starting watch" in {
				write(tempFile)
				observer.waitForAnswer shouldReturn FileModified(tempFile)
			}

			"but only notify once" in {
				val f = tempDir addFile "foo.txt"
				observer.waitForAnswer
				write(f)
				observer.waitForAnswer shouldReturn FileModified(f)
				observer shouldBe empty
			}
		}

		"handle file events" - {
			"normal" in {
				val f = tempDir addFile "foo.txt"
				observer.waitForAnswer shouldReturn FileEvent(f)
			}
			"case match" in {
				val f = tempDir addFile "foo.txt"
				observer.waitForAnswer match {
					case FileEvent(f2) => f2 shouldReturn f
				}
			}
		}
	}
}
