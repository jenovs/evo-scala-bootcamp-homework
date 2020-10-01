package ControlStructures

import Homework._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HomeworkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  // DIVIDE
  "Divide" should "do division" in {
    process("divide 36 4") shouldEqual "36 divided by 4 is 9"
  }

  "Divide" should "show error when divide by 0" in {
    process("divide 36 0") shouldEqual "Error: Can't divide by 0"
  }

  "Divide" should "show error when only one argument provided" in {
    process("divide 36") shouldEqual "Error: We need two numbers to divide"
  }

  "Divide" should "ignore extra numbers" in {
    process("divide 36 9 15 55") shouldEqual "36 divided by 9 is 4"
  }

  // SUM
  "Sum" should "do addition" in {
    process("sum 4 5") shouldEqual "the sum of 4 5 is 9"
  }

  "Sum" should "work with extra spaces" in {
    process("sum 4  5      8") shouldEqual "the sum of 4 5 8 is 17"
  }

  "Sum" should "work with invalid chars" in {
    process("sum 4 foo 5 bar 1") shouldEqual "the sum of 4 5 1 is 10"
  }

  "Sum" should "return error when no arguments provided" in {
    process("sum  ") shouldEqual "Error: No numbers provided"
  }

  // AVERAGE
  "Average" should "calculate average" in {
    process("average 3 8 1") shouldEqual "the average of 3 8 1 is 4"
  }

  // MIN
  "Min" should "return min value" in {
    process("min 3 8 1 6") shouldEqual "the minimum of 3 8 1 6 is 1"
  }

  "Min" should "work with negative values" in {
    process("min -3 -8 -1 -6") shouldEqual "the minimum of -3 -8 -1 -6 is -8"
  }

  // MAX
  "Max" should "return max value" in {
    process("max 3 8 1 6") shouldEqual "the maximum of 3 8 1 6 is 8"
  }

  "Max" should "work with negative values" in {
    process("max -3 -8 -1 -6") shouldEqual "the maximum of -3 -8 -1 -6 is -1"
  }

  // ERROR
  "Error" should "be displayed if unrecognized command" in {
    process("foo 5 6") shouldEqual "Error: Unknown command"
  }
}
