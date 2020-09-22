package basics

import Homework._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HomeworkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "lcm" should "calculate lcm" in {
    lcm(2, 3) shouldEqual 6
    lcm(4, 6) shouldEqual 12
    lcm(12, 18) shouldEqual 36
    lcm(21, 6) shouldEqual 42
    lcm(30, 45) shouldEqual 90
    lcm(17, 32) shouldEqual 544
    lcm(42, 69) shouldEqual 966
    lcm(123, 456) shouldEqual 18696
  }

  "gcd" should "calculate gcd" in {
    gcd(8, 12) shouldEqual 4
    gcd(54, 24) shouldEqual 6
    gcd(48, 180) shouldEqual 12
  }
}
