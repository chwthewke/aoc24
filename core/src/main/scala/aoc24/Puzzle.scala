package aoc24

import cats.Id
import scala.concurrent.duration._

abstract class Puzzle[F[_]]( val n: Int ) {
  def timeout: Duration = 1.minute

  def run( input: Input ): F[String]

  def runBonus( input: Input ): F[String]
}

object Puzzle {
  type Pure = Puzzle[Id]
}
