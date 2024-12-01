package aoc24

case class Input( rawLines: Vector[String] ) {
  def lines: Vector[String] = rawLines.filter( _.nonEmpty )
  def trimmed: String       = lines.mkString( "\n" )
  def raw: String           = rawLines.mkString( "\n" )
}
