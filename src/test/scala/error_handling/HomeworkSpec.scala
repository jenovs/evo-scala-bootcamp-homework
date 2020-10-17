package error_handling

import Homework._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import cats.data.Validated.Valid
import cats.data.Validated.Invalid

class HomeworkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  it should "create a valid card 1" in {
    PaymentCardValidator.validate("John Doe", "4556737586899855", "12/22", "666").isValid shouldBe true
  }

  it should "catch missing name" in {
    val card = PaymentCardValidator.validate("", "4556737586899855", "12/22", "666")

    card match {
      case Invalid(e) => {
        val errors = e.toNonEmptyList.toList.map(_.toString)
        errors.length shouldBe 1
        errors(0) shouldBe "Card name must be min 2 characters"
      }
    }
  }

  it should "catch invalid card number" in {
    val card = PaymentCardValidator.validate("John Doe", "4556737586899859", "12/22", "666")

    card match {
      case Invalid(e) => {
        val errors = e.toNonEmptyList.toList.map(_.toString)
        errors.length shouldBe 1
        errors(0) shouldBe "Card number invalid"
      }
    }
  }

  it should "ignore whitespaces in card number" in {
    val card = PaymentCardValidator.validate("John Doe", "4556 7375 8689 9855", "12/22", "666")
    println(card)
    card.isValid shouldBe true

  }

  it should "catch invalid date" in {
    val card = PaymentCardValidator.validate("John Doe", "4556737586899855", "17/18", "666")

    card match {
      case Invalid(e) => {
        val errors = e.toNonEmptyList.toList.map(_.toString)
        errors.length shouldBe 2
        errors(0) shouldBe "Invalid expiration month"
        errors(1) shouldBe "Invalid expiration year"
      }
    }
  }

  it should "catch invalid security code" in {
    val card = PaymentCardValidator.validate("John Doe", "4556737586899855", "11/22", "6A")

    card match {
      case Invalid(e) => {
        val errors = e.toNonEmptyList.toList.map(_.toString)
        errors.length shouldBe 1
        errors(0) shouldBe "Security code must be 3 digits long"
      }
    }
  }

  it should "catch multiple errors" in {
    val card = PaymentCardValidator.validate("", "455673758689985", "19/22", "6A")

    card match {
      case Invalid(e) => {
        val errors = e.toNonEmptyList.toList.map(_.toString)
        errors.length shouldBe 4
        errors(0) shouldBe "Card name must be min 2 characters"
        errors(1) shouldBe "Card number must be 16 digits long"
        errors(2) shouldBe "Invalid expiration month"
        errors(3) shouldBe "Security code must be 3 digits long"
      }
    }
  }
}
