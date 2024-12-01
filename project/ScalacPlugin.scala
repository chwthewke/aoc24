import sbt._
import sbt.Keys._

object ScalacPlugin extends AutoPlugin {
  val options: Seq[String] = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Ywarn-macros:after",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )

  // TODO keeping this around, but not sure it's still needed
  // def workaroundForIntellij( opts: Seq[String] ): Seq[String] =
  //   if (sys.props.contains( "idea.runid" ))
  //     forTest( opts )
  //   else
  //     opts

  def forTest( opts: Seq[String] ): Seq[String] =
    opts.filterNot( _ == "-Ywarn-value-discard" )

  def forConsole( opts: Seq[String] ): Seq[String] =
    opts.filterNot( Set( "-Xfatal-warnings", "-Xlint", "-Ywarn-unused-import" ) )

  override def projectSettings: Seq[Def.Setting[_]] =
    // format: off
    Seq(
      // scalacOptions                         ++= workaroundForIntellij( options ),
      scalacOptions                     ++= options,
      Test / scalacOptions              ~=  forTest,
      Compile / console / scalacOptions ~=  forConsole,
      Test / console / scalacOptions    :=  forTest( (Compile / console / scalacOptions).value ),
      Test / testOptions                +=  Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
    )
  // format: on
}
