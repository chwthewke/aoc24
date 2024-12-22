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
        """version = "3.7.17"
          |runner.dialect = scala213
          |
          |preset = defaultWithAlign
          |maxColumn = 120
          |lineEndings = preserve
          |
          |assumeStandardLibraryStripMargin = true
          |align.arrowEnumeratorGenerator = true
          |docstrings.style = Asterisk
          |spaces.inParentheses = true
          |
          |newlines.beforeCurlyLambdaParams = multilineWithCaseOnly
          |newlines.avoidForSimpleOverflow = [slc]
          |
          |rewrite.rules = [Imports]
          |rewrite.imports.expand = true
          |
          |literals.hexPrefix=Lower
          |literals.hexDigits=Upper
          |""".stripMargin
      )
    },
    scalafmtConfig := {
      val _ = scalafmtGenerateConfig.value
      file( ".scalafmt.conf" )
    }
  )
}
