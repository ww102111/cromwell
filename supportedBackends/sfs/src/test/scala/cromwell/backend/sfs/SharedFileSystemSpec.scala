package cromwell.backend.sfs

import java.nio.file.{FileSystems, Files}

import better.files._
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.specs2.mock.Mockito
import wdl4s.values.WdlFile

class SharedFileSystemSpec extends FlatSpec with Matchers with Mockito with TableDrivenPropertyChecks {

  behavior of "SharedFileSystem"

  val defaultLocalization = ConfigFactory.parseString(""" localization: [copy, hard-link, soft-link] """)
  val hardLinkLocalization = ConfigFactory.parseString(""" localization: [hard-link] """)
  val softLinkLocalization = ConfigFactory.parseString(""" localization: [soft-link] """)
  val localFS = List(FileSystems.getDefault)


  def localizationTest(config: Config,
                       docker: Boolean,
                       fileInCallDir: Boolean = false,
                       fileAlreadyExists: Boolean = false,
                       symlink: Boolean = false,
                       linkNb: Int = 1) = {
    val callDir = File.newTemporaryDirectory("SharedFileSystem")
    val orig = if (fileInCallDir) callDir.createChild("inputFile") else File.newTemporaryFile("inputFile")
    val dest = if (fileInCallDir) orig else callDir./(orig.pathAsString.drop(1))
    orig.touch()
    if (fileAlreadyExists) {
      dest.parent.createDirectories()
      dest.touch()
    }

    val inputs = Map("input" -> WdlFile(orig.pathAsString))
    val sharedFS = new SharedFileSystem { override val sharedFileSystemConfig = config }
    val result = sharedFS.localizeInputs(callDir.path, docker = docker, localFS, inputs)

    result.isSuccess shouldBe true
    result.get should contain theSameElementsAs Map("input" -> WdlFile(dest.pathAsString))

    dest.exists shouldBe true
    countLinks(dest) should be(linkNb)
    isSymLink(dest) should be(symlink)

    orig.delete(swallowIOExceptions = true)
    dest.delete(swallowIOExceptions = true)
  }

  it should "not localize a file already in the call root" in {
    localizationTest(defaultLocalization, docker = false, fileInCallDir = true)
    localizationTest(defaultLocalization, docker = true, fileInCallDir = true)
  }

  it should "not localize a file already localized" in {
    localizationTest(defaultLocalization, docker = false, fileAlreadyExists = true)
    localizationTest(defaultLocalization, docker = true, fileAlreadyExists = true)
  }

  it should "localize a file via copy" in {
    localizationTest(defaultLocalization, docker = false)
    localizationTest(defaultLocalization, docker = true)
  }

  it should "localize a file via hard link" in {
    localizationTest(hardLinkLocalization, docker = false, linkNb = 2)
    localizationTest(hardLinkLocalization, docker = true, linkNb = 2)
  }

  it should "localize a file via symbolic link" in {
    localizationTest(softLinkLocalization, docker = false, symlink = true)
  }

  private[this] def countLinks(file: File): Int = Files.getAttribute(file.path, "unix:nlink").asInstanceOf[Int]

  private[this] def isSymLink(file: File): Boolean = Files.isSymbolicLink(file.path)
}
