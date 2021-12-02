package ch.bharanya

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class UtilTest extends AnyFlatSpec with should.Matchers {
  "util" should "read files from res" in {
    Util.getFileLines(1) should have length 2000
  }

}
