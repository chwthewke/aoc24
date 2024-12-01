import sbt._
import sbt.Keys._
import sbtbuildinfo._
import sbtbuildinfo.BuildInfoKeys._

object SbtBuildInfoPlugin extends AutoPlugin {
  override def requires: Plugins = super.requires && BuildInfoPlugin

  val shortVersion = SettingKey[String]( "short-version" ).withRank( KeyRanks.Invisible )

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      buildInfoPackage := "buildinfo",
      buildInfoObject := "Aoc24",
      shortVersion := shortenVersion( version.value ),
      buildInfoKeys ++= Seq[BuildInfoKey]( name, version, shortVersion, scalaVersion )
    )

  val MajorMinor = raw"(\d+)\.(\d+).*".r

  private def shortenVersion( version: String ): String =
    version match {
      case MajorMinor( maj, min ) => s"$maj.$min"
      case _                      => throw new IllegalArgumentException( s"Could not parse version $version" )
    }

}
