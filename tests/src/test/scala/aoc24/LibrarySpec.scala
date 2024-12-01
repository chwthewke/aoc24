package aoc24

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LibrarySpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
  "function" should {
    "return 42" in {
      Library.function must ===( 42 )
    }
  }
}
