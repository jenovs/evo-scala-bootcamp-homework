package error_handling

import cats.data.Validated.Valid
import cats.data.Validated.Invalid

object Homework extends App {
  // Homework 7.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  final case class PaymentCard private (
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String
  )
  object PaymentCard {
    def create(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String
    ) = {
      PaymentCard(name, number, expirationDate, securityCode)
    }
  }

  sealed trait ValidationError
  object ValidationError {
    final case object CardNameTooShort extends ValidationError {
      override def toString: String = "Card name must be min 2 characters"
    }
    final case object CardNameTooLong extends ValidationError {
      override def toString: String = "Card name must be max 26 characters"
    }
    final case object CardNumberWrongLength extends ValidationError {
      override def toString: String = "Card number must be 16 digits long"
    }
    final case object CardNumberInvalid extends ValidationError {
      override def toString: String = "Card number invalid"
    }
    final case object ExpirationDateFormatInvalid extends ValidationError {
      override def toString: String = "Expiration date must have format MM/YY or MM-YY"
    }
    final case object ExpirationDateMonthInvalid extends ValidationError {
      override def toString: String = "Invalid expiration month"
    }
    final case object ExpirationDateYearInvalid extends ValidationError {
      override def toString: String = "Invalid expiration year"
    }
    final case object SecurityCodeInvalid extends ValidationError {
      override def toString: String = "Security code must be 3 digits long"
    }
  }

  object PaymentCardValidator {
    import cats.data.ValidatedNec
    import cats.syntax.all._

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    /**
      * [[name]] must be from min 2 to max 26 characters long
      *
      * [[number]] 16 digits exactly (sorry AMEX)
      *
      * [[expirationDate]] in format MM/YY or MM-YY where YY >= 20
      *
      * [[securityCode]] three digits exactly (sorry AMEX again)
      */
    def validate(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String
    ): AllErrorsOr[PaymentCard] = {
      def validateName: AllErrorsOr[String] = {
        val nameTrimmed = name.trim.replaceAll("\\s+", " ")

        def checkMinLength: AllErrorsOr[String] = {
          if (nameTrimmed.length >= 2) nameTrimmed.validNec
          else CardNameTooShort.invalidNec
        }

        def checkMaxLength: AllErrorsOr[String] = {
          if (nameTrimmed.length <= 26) nameTrimmed.validNec
          else CardNameTooLong.invalidNec
        }

        checkMinLength *> checkMaxLength
      }

      def validateNumber: AllErrorsOr[String] = {
        def isLuhnValid(s: String): Boolean = {
          s.init.reverse.zipWithIndex.map {
            case (char, i) => {
              if (i % 2 == 0) {
                val res = char.asDigit * 2
                if (res > 9) res - 9 else res
              } else char.asDigit
            }
          }.sum % 10 == s.takeRight(1).toInt
        }

        def validateNumberLength: AllErrorsOr[String] = {
          val numberTrimmed = number.trim.replaceAll("\\s", "")
          println(numberTrimmed)

          if (numberTrimmed.matches("\\d{16}") && numberTrimmed.length == 16) numberTrimmed.validNec
          else CardNumberWrongLength.invalidNec
        }

        def validateNumberFormat(number: String): AllErrorsOr[String] =
          if (isLuhnValid(number)) number.validNec
          else CardNumberInvalid.invalidNec

        validateNumberLength andThen validateNumberFormat
      }

      def validateDate: AllErrorsOr[String] = {
        val dateTrimmed = expirationDate.trim.replaceAll("\\s", "")

        def validateDateFormat: AllErrorsOr[String] =
          if (dateTrimmed.matches("\\d{2}(\\/|-)\\d{2}")) dateTrimmed.validNec
          else ExpirationDateFormatInvalid.invalidNec

        def validateMonthAndYear(date: String) = {
          val month = date.take(2).toInt
          val year = date.takeRight(2).toInt

          def validateMonth(month: Int): AllErrorsOr[String] = {
            if (month < 0 || month > 12) ExpirationDateMonthInvalid.invalidNec
            else month.toString.validNec
          }

          def validateYear(year: Int): AllErrorsOr[String] = {
            if (year < 20) ExpirationDateYearInvalid.invalidNec
            else year.toString.validNec
          }

          (validateMonth(month), validateYear(year)).mapN((month, year) => s"$month/$year")
        }

        validateDateFormat andThen validateMonthAndYear
      }

      def validateSecurityCode: AllErrorsOr[String] = {
        val securityCodeTrimmed = securityCode.trim
        if (securityCodeTrimmed.length != 3 || !securityCodeTrimmed.matches("\\d{3}"))
          SecurityCodeInvalid.invalidNec
        else securityCodeTrimmed.validNec

      }

      (validateName, validateNumber, validateDate, validateSecurityCode).mapN(PaymentCard.create)
    }
  }
}
