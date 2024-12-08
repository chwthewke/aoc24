import sbt._
import sbt.Keys._

// format: off
ThisBuild / organization      := "net.chwthewke"
ThisBuild / scalaOrganization := "org.scala-lang"
ThisBuild / scalaVersion      := "2.13.15"
// TODO when I can make sense of lm-coursier
ThisBuild / conflictManager                        := ConflictManager.strict
ThisBuild / updateSbtClassifiers / conflictManager := ConflictManager.default
// format: on

enablePlugins( FormatPlugin, DependenciesPlugin )

val `aoc24-core` = project
  .in( file( "core" ) )
  .settings( libraryDependencies ++= kindProjector )
  .settings( libraryDependencies ++= cats ++ catsEffect ++ catsParse ++ enumeratum ++ kittens ++ fs2 ++ algebra )
  .enablePlugins( SbtBuildInfoPlugin, ScalacPlugin )

val `aoc24-app` = project
  .in( file( "app" ) )
  .settings( Compile / run / fork := true )
  .settings( libraryDependencies ++= kindProjector )
  .settings( libraryDependencies ++= fs2 )
  .dependsOn( `aoc24-core` )
  .enablePlugins( SbtBuildInfoPlugin, ScalacPlugin )

val `aoc24-tests` = project
  .in( file( "tests" ) )
  .settings( libraryDependencies ++= kindProjector )
  .settings( libraryDependencies ++= scalatest ++ scalacheck )
  .dependsOn( `aoc24-core`, `aoc24-app` )
  .enablePlugins( ScalacPlugin )

val `aoc24` = project
  .in( file( "." ) )
  .aggregate(
    `aoc24-core`,
    `aoc24-app`,
    `aoc24-tests`
  )
