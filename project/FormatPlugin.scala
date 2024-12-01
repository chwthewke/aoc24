import sbt._
import sbt.Keys._
import org.scalafmt.sbt.ScalafmtPlugin
import org.scalafmt.sbt.ScalafmtPlugin.autoImport._

object FormatPlugin extends AutoPlugin {

  override def requires: Plugins = ScalafmtPlugin

  val scalafmtGenerateConfig: TaskKey[Unit] =
    TaskKey[Unit]( "scalafmtGenerateConfig" )

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtOnCompile := !sys.props.contains( "idea.runid" )
  )

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtGenerateConfig := {
      IO.write(
        file( ".scalafmt.conf" ),
        """version = "2.0.0"
          |
          |style = defaultWithAlign
          |maxColumn = 120
          |lineEndings = preserve
          |
          |assumeStandardLibraryStripMargin = true
          |align.arrowEnumeratorGenerator = true
          |spaces.inParentheses = true
          |
          |rewrite.rules = [ExpandImportSelectors]
          |""".stripMargin
      )
    },
    scalafmtConfig := {
      val _ = scalafmtGenerateConfig.value
      file( ".scalafmt.conf" )
    }
  )
}
