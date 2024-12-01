import sbt._
import sbt.Keys._
import sbt.librarymanagement.DependencyBuilders

object DependenciesPlugin extends AutoPlugin {
  type Deps = Seq[ModuleID]

  object autoImport {
    type Deps = DependenciesPlugin.Deps
    import scala.language.implicitConversions

    implicit def ToStringOps( orgName: String ): StringOps = new StringOps( orgName )

    implicit def ToDbOanOps( dbOans: Seq[DbOan] ): DbOanOps = new DbOanOps( dbOans )

    implicit def ToGroupOps( deps: Deps ): GroupOps = new GroupOps( deps )

    val kindProjector: Deps =
      Seq( compilerPlugin( "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full ) )

    val betterMonadicFor: Deps = Seq( compilerPlugin( "com.olegpy" %% "better-monadic-for" % "0.3.1" ) )

    val catsVersion         = "2.10.0"
    val cats: Deps          = "org.typelevel" %% Seq( "cats-core", "cats-kernel" ) % catsVersion
    val catsFree: Deps      = Seq( "org.typelevel" %% "cats-free" % catsVersion )
    val catsMtl: Deps       = Seq( "org.typelevel" %% "cats-mtl" % "1.4.0" )
    val mouse: Deps         = Seq( "org.typelevel" %% "mouse" % "1.2.2" )
    val kittens: Deps       = Seq( "org.typelevel" %% "kittens" % "3.1.0" )
    val alleycatsCore: Deps = Seq( "org.typelevel" %% "alleycats-core" % catsVersion )

    val catsEffect: Deps = "org.typelevel" %% Seq( "cats-effect", "cats-effect-kernel", "cats-effect-std" ) % "3.5.2"

    val fs2: Deps = "co.fs2" %% Seq( "fs2-core", "fs2-io" ) % "3.9.3"

    val http4sVersion           = "0.23.24"
    val http4s: Deps            = "org.http4s" %% Seq( "http4s-dsl", "http4s-core", "http4s-circe" ) % http4sVersion
    val http4sClientCore: Deps  = Seq( "org.http4s" %% "http4s-client" % http4sVersion )
    val http4sServerCore: Deps  = Seq( "org.http4s" %% "http4s-server" % http4sVersion )
    val http4sEmberServer: Deps = Seq( "org.http4s" %% "http4s-ember-server" % http4sVersion )
    val http4sEmberClient: Deps = Seq( "org.http4s" %% "http4s-ember-client" % http4sVersion )

    val monocleVersion       = "3.2.0"
    val monocle: Deps        = "dev.optics" %% Seq( "monocle-core", "monocle-macro" ) % monocleVersion
    val monocleState: Deps   = Seq( "dev.optics" %% "monocle-state" % monocleVersion )
    val monocleGeneric: Deps = Seq( "dev.optics" %% "monocle-generic" % monocleVersion )
    val monocleUnsafe: Deps  = Seq( "dev.optics" %% "monocle-unsafe" % monocleVersion )

    val circeVersion      = "0.14.6"
    val circe: Deps       = "io.circe" %% Seq( "circe-core", "circe-generic", "circe-parser" ) % circeVersion
    val circeOptics: Deps = Seq( "io.circe" %% "circe-optics" % "0.14.1" )
    val circeFs2: Deps    = Seq( "io.circe" %% "circe-fs2" % "0.14.1" )
    val circeJawn: Deps   = Seq( "io.circe" %% "circe-jawn" % circeVersion )
    val jawnParser: Deps  = Seq( "org.typelevel" %% "jawn-parser" % "1.4.0" )

    val scodec: Deps = Seq(
      "org.scodec" %% "scodec-bits" % "1.1.38",
      "org.scodec" %% "scodec-core" % "1.11.10"
    )

    val catsParse: Deps = Seq( "org.typelevel" %% "cats-parse" % "1.0.0" )

    val atto: Deps = Seq( "org.tpolecat" %% "atto-core" % "0.9.5" )

    val asciiGraphs: Deps = Seq( "org.scalameta" %% "ascii-graphs" % "0.1.2" )
    val graphs: Deps      = "com.flowtick" %% Seq( "graphs-core", "graphs-cats" ) % "0.9.0"

    val spire: Deps    = Seq( "org.typelevel" %% "spire"         % "0.18.0" )
    val algebird: Deps = Seq( "com.twitter"   %% "algebird-core" % "0.13.10" )
    val algebra: Deps  = Seq( "org.typelevel" %% "algebra"       % "2.10.0" )

    val breeze: Deps = "org.scalanlp" %% Seq( "breeze", "breeze-natives" ) % "2.1.0"
    val breezeDependencyOverrides: Deps =
      Seq(
        "org.apache.commons"       % "commons-math3" % "3.6.1",
        "com.github.fommil.netlib" % "core"          % "1.1.2"
      )

    val ojAlgo: Deps = Seq( "org.ojalgo" % "ojalgo" % "53.1.1" )

    val enumeratum: Deps =
      Seq( "com.beachape" %% "enumeratum" % "1.7.2", "com.beachape" %% "enumeratum-cats" % "1.7.3" )
    val enumeratumCirce: Deps = Seq( "com.beachape" %% "enumeratum-circe" % "1.7.3" )

    val shapeless: Deps = Seq( "com.chuusai" %% "shapeless" % "2.3.10" )

    val java8compat: Deps = Seq( "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2" )
    val scalaXml: Deps    = Seq( "org.scala-lang.modules" %% "scala-xml"          % "2.2.0" )

    val scalaCollectionsCompat: Deps = Seq( "org.scala-lang.modules" %% "scala-collection-compat" % "2.11.0" )

    val log4cats: Deps =
      Seq(
        "org.slf4j"     % "slf4j-api"       % "1.7.36",
        "org.typelevel" %% "log4cats-slf4j" % "2.6.0"
      )

    val logback: Deps = Seq( "ch.qos.logback" % "logback-classic" % "1.3.14" )

    val natchezVersion       = "0.3.4"
    val natchezCore: Deps    = Seq( "org.tpolecat" %% "natchez-core" % natchezVersion )
    val natchezNoop: Deps    = Seq( "org.tpolecat" %% "natchez-noop" % natchezVersion )
    val natchezLog: Deps     = Seq( "org.tpolecat" %% "natchez-log" % natchezVersion )
    val natchezDatadog: Deps = Seq( "org.tpolecat" %% "natchez-datadog" % natchezVersion )
    val natchezJaeger: Deps  = Seq( "org.tpolecat" %% "natchez-jaeger" % natchezVersion )
    val natchezHttp4s: Deps  = Seq( "org.tpolecat" %% "natchez-http4s" % "0.5.0" )

    val natchezOverrides: Deps = Seq(
      "com.squareup.okhttp3"   % "okhttp"                     % "4.9.0",
      "com.squareup.okio"      % "okio"                       % "2.8.0",
      "org.jetbrains.kotlin"   % "kotlin-stdlib-common"       % "1.4.10",
      "org.jetbrains.kotlin"   % "kotlin-stdlib"              % "1.4.10",
      "io.opentracing"         % "opentracing-api"            % "0.33.0",
      "io.opentracing"         % "opentracing-noop"           % "0.33.0",
      "io.opentracing.contrib" % "opentracing-tracerresolver" % "0.1.8",
      "io.opentracing"         % "opentracing-util"           % "0.33.0"
    )

    val pureconfigVersion = "0.17.4"
    val pureconfig: Deps = "com.github.pureconfig" %% Seq(
      "pureconfig-core",
      "pureconfig-cats",
      "pureconfig-generic"
    ) % pureconfigVersion

    val pureconfigEnumeratum: Deps = Seq( "com.github.pureconfig" %% "pureconfig-enumeratum"  % pureconfigVersion )
    val pureconfigCatsEffect: Deps = Seq( "com.github.pureconfig" %% "pureconfig-cats-effect" % pureconfigVersion )
    val pureconfigFs2: Deps        = Seq( "com.github.pureconfig" %% "pureconfig-fs2"         % pureconfigVersion )
    val pureconfigHttp4s: Deps     = Seq( "com.github.pureconfig" %% "pureconfig-http4s"      % pureconfigVersion )

    private[DependenciesPlugin] val typesafeConfig: Deps = Seq( "com.typesafe" % "config" % "1.4.3" )

    val decline: Deps = "com.monovore" %% Seq( "decline", "decline-effect" ) % "2.4.1"

    val doobieVersion             = "0.13.4"
    val doobie: Deps              = "org.tpolecat" %% Seq( "doobie-core", "doobie-free" ) % doobieVersion
    val doobiePostgres: Deps      = "org.tpolecat" %% Seq( "doobie-postgres", "doobie-hikari" ) % doobieVersion
    val doobiePostgresCirce: Deps = Seq( "org.tpolecat" %% "doobie-postgres-circe" % doobieVersion )
    val doobieH2: Deps            = Seq( "org.tpolecat" %% "doobie-h2" % doobieVersion )
    val doobieScalatest: Deps     = Seq( "org.tpolecat" %% "doobie-scalatest" % doobieVersion )

    val postgresql: Deps = Seq( "org.postgresql" % "postgresql"  % "42.7.0" )
    val flywayCore: Deps = Seq( "org.flywaydb"   % "flyway-core" % "10.1.0" )

    val scalatest: Deps = Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.17",
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0"
    )

    val scalacheck: Deps =
      Seq(
        "org.scalacheck"    %% "scalacheck"      % "1.17.0",
        "io.chrisdavenport" %% "cats-scalacheck" % "0.3.2"
      )

    val uTest: Deps = Seq( "com.lihaoyi" %% "utest" % "0.8.2" )

    val uTestRunner: Def.Setting[_] =
      testFrameworks += new TestFramework( "utest.runner.Framework" )

    val autoDiffVersion          = "0.6.0"
    val autoDiff: Deps           = "fr.thomasdufour" %% Seq( "auto-diff-core", "auto-diff-generic" ) % autoDiffVersion
    val autoDiffEnumeratum: Deps = Seq( "fr.thomasdufour" %% "auto-diff-enumeratum" % autoDiffVersion )
    val autoDiffScalatest: Deps  = Seq( "fr.thomasdufour" %% "auto-diff-scalatest" % autoDiffVersion )
  }

  import autoImport._

  def allModules: Deps = {
    kindProjector ++
      betterMonadicFor ++
      cats ++
      catsFree ++
      catsMtl ++
      catsEffect ++
      catsParse ++
      mouse ++
      kittens ++
      alleycatsCore ++
      fs2 ++
      http4s ++
      http4sServerCore ++
      http4sEmberServer ++
      http4sClientCore ++
      http4sEmberClient ++
      monocle ++
      monocleState ++
      monocleGeneric ++
      monocleUnsafe ++
      circe ++
      circeFs2 ++
      circeJawn ++
      circeOptics ++
      scodec ++
      atto ++
      asciiGraphs ++
      graphs ++
      algebra ++
      spire ++
      algebird ++
      breeze ++
      breezeDependencyOverrides ++
      ojAlgo ++
      enumeratum ++
      enumeratumCirce ++
      shapeless ++
      java8compat ++
      jawnParser ++
      scalaXml ++
      scalaCollectionsCompat ++
      log4cats ++
      logback ++
      natchezCore ++
      natchezNoop ++
      natchezLog ++
      natchezDatadog ++
      natchezJaeger ++
      natchezHttp4s ++
      natchezOverrides ++
      pureconfig ++
      pureconfigEnumeratum ++
      pureconfigFs2 ++
      pureconfigHttp4s ++
      typesafeConfig ++
      decline ++
      doobie ++
      doobiePostgres ++
      doobiePostgresCirce ++
      doobieH2 ++
      doobieScalatest ++
      postgresql ++
      flywayCore ++
      scalatest ++
      scalacheck ++
      uTest ++
      autoDiff ++
      autoDiffEnumeratum ++
      autoDiffScalatest
  }

  override def buildSettings: Seq[Def.Setting[_]] =
    ThisBuild / dependencyOverrides ++= Seq(
      "org.scala-lang" % "scala-library"  % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect"  % scalaVersion.value
    ) ++ allModules

  type DbOan = DependencyBuilders.OrganizationArtifactName

  class GroupOps( val self: Seq[ModuleID] ) extends AnyVal {
    def exclude( org: String, name: String ): Seq[ModuleID] =
      self.map( _.exclude( org, name ) )

    def %( configurations: String ): Seq[ModuleID] =
      self.map( _ % configurations )

    def classifier( c: String ): Seq[ModuleID] =
      self.map( _ classifier c )
  }

  class StringOps( val self: String ) extends AnyVal {
    def %%( artifactIds: Seq[String] ): Seq[DbOan] = artifactIds.map( self %% _ )
    def %( artifactIds: Seq[String] ): Seq[DbOan]  = artifactIds.map( self % _ )
  }

  class DbOanOps( val self: Seq[DbOan] ) extends AnyVal {
    def %( revision: String ): Deps = self.map( _ % revision )
  }

}
