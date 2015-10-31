package codpiece

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

class UserActionTest extends FlatSpec with Matchers {

  "basic square tests" should "pass" in {
    Codpiece.squares.size shouldBe 64
    Codpiece.rankToChar(1) shouldBe '7'
    Codpiece.fileToChar(1) shouldBe 'b'
  }
}