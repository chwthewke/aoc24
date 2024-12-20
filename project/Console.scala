import sbt._
import sbt.Keys._

object Console {

  case class Imports( compileList: List[String], testList: List[String] ) {
    def depends( others: Imports* ): Imports =
      Imports(
        others.foldRight( compileList )( _.compileList ++ _ ),
        others.foldRight( testList )( _.testList ++ _ )
      )

    def compileS = compileList.map( "import " + _ ).mkString( "\n" )
    def testS    = (compileList ++ testList).map( "import " + _ ).mkString( "\n" )

    def settings = Seq(
      initialCommands := compileS,
      Test / initialCommands := testS
    )
  }

  val coreImports = Imports(
    "aoc24._" :: "cats._" :: "cats.data._" :: "cats.implicits._" :: Nil,
    "org.scalacheck.Gen" :: "org.scalacheck.Gen._" :: Nil
  )
}
