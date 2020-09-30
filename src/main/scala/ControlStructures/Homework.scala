package ControlStructures

import scala.io.Source
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.{util => ju}

object Homework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command {
    def name: String
    def calculate: Either[ErrorMessage, Result]
  }

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command {
      val name = "divide"
      def calculate: Either[ErrorMessage, Result] = {
        if (divisor == 0) Left(ErrorMessage("Can't divide by 0"))
        else
          Right(Result(name, List(dividend, divisor), dividend / divisor))
      }
    }

    final case class Sum(numbers: List[Double]) extends Command {
      override val name = "sum"
      def calculate: Either[ErrorMessage, Result] = {
        if (numbers.isEmpty) Left(ErrorMessage("No numbers provided"))
        else
          Right(Result(name, numbers, numbers.foldLeft(0.0)((a, b) => a + b)))
      }
    }

    final case class Average(numbers: List[Double]) extends Command {
      val name = "average"
      def calculate: Either[ErrorMessage, Result] = {
        val sum = Command.Sum(numbers).calculate
        sum match {
          case Right(value) =>
            Right(Result(name, numbers, (value.result / numbers.length)))
          case Left(err) => Left(ErrorMessage(err.value))
        }
      }
    }

    final case class Min(numbers: List[Double]) extends Command {
      val name = "min"
      def calculate: Either[ErrorMessage, Result] = {
        if (numbers.isEmpty) Left(ErrorMessage("No numbers provided"))
        else Right(Result(name, numbers, numbers.reduce((a, b) => a min b)))
      }
    }

    final case class Max(numbers: List[Double]) extends Command {
      val name = "max"
      def calculate: Either[ErrorMessage, Result] = {
        if (numbers.isEmpty) Left(ErrorMessage("No numbers provided"))
        else Right(Result(name, numbers, numbers.reduce((a, b) => a max b)))
      }
    }
  }

  final case class ErrorMessage(value: String)

  final case class Result(
      command: String,
      numbers: List[Double],
      result: Double
  ) {}

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
    def parseNums(n: List[String]) =
      n.map(_.toDoubleOption.getOrElse(Double.NaN)).filter(n => !n.isNaN)

    x.split("\\s+").toList match {
      case x :: xs if x == "divide" => {
        val nums = parseNums(xs)
        if (nums.length < 2) Left(ErrorMessage("We need two numbers to divide"))
        else {
          val List(dividend, divisor) = nums.slice(0, 2)
          Right(Command.Divide(dividend, divisor))
        }
      }
      case x :: xs if x == "sum"     => Right(Command.Sum(parseNums(xs)))
      case x :: xs if x == "average" => Right(Command.Average(parseNums(xs)))
      case x :: xs if x == "min"     => Right(Command.Min(parseNums(xs)))
      case x :: xs if x == "max"     => Right(Command.Max(parseNums(xs)))
      case x :: xs if x.isBlank      => Left(ErrorMessage("Enter command"))
      case _                         => Left(ErrorMessage("Unknown command"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x.calculate
  }

  def fmt(number: Double) = {
    // https://stackoverflow.com/a/25308216/6696407
    val df =
      new DecimalFormat(
        "0",
        DecimalFormatSymbols.getInstance(ju.Locale.ENGLISH)
      )
    df.setMaximumFractionDigits(340)

    df.format(number)
  }

  def renderResult(x: Result): String = {
    val numbersString = x.numbers.map(fmt).mkString(" ")

    x.command match {
      case "divide" =>
        s"${fmt(x.numbers(0))} divided by ${fmt(x.numbers(1))} is ${fmt(x.result)}"
      case "sum"     => s"the sum of $numbersString is ${fmt(x.result)}"
      case "average" => s"the average of $numbersString is ${fmt(x.result)}"
      case "min"     => s"the minimum of $numbersString is ${fmt(x.result)}"
      case "max"     => s"the maximum of $numbersString is ${fmt(x.result)}"
      case _         => "¯\\_(ツ)_/¯"
    }
  }

  def process(x: String): String = {
    // import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    val result = for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result

    result match {
      case Right(res) => renderResult(res)
      case Left(err)  => s"Error: ${err.value}"
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println
}
