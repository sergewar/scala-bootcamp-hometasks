package com.sss.scalabootcamp.hometasks.task3

import scala.io.Source

object ControlStructuresHomework {

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

  final case class ErrorMessage(value: String)

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  sealed trait Result

  object Result {

    final case class DivideResult(dividend: Double, divisor: Double, value: Double) extends Result

    final case class SumResult(numbers: List[Double], value: Double) extends Result

    final case class AverageResult(numbers: List[Double], value: Double) extends Result

    final case class MinResult(numbers: List[Double], value: Double) extends Result

    final case class MaxResult(numbers: List[Double], value: Double) extends Result

  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???
    // Consider how to handle extra whitespace gracefully (without errors).
    import com.sss.scalabootcamp.hometasks.task3.ControlStructuresHomework.Command._

    val input = x.split("\\s+").toList

    input match {
      case i if i.length < 2                                                            => Left(ErrorMessage("Not enough arguments for calculate"))
      case _ :: xs if xs.length != xs.map(_.toDoubleOption).filterNot(_.isEmpty).length => Left(ErrorMessage("Can not convert to numbers"))
      case x :: xs if x == "divide"                                                     =>
        xs match {
          case xs if xs.length != 2 => Left(ErrorMessage("Need exactly two numbers for divide"))
          case _                    => Right(Divide(xs.head.toDouble, xs.tail.head.toDouble))
        }
      case x :: xs if x == "sum"                                                        => Right(Sum(xs.map(_.toDouble)))
      case x :: xs if x == "average"                                                    => Right(Average(xs.map(_.toDouble)))
      case x :: xs if x == "min"                                                        => Right(Min(xs.map(_.toDouble)))
      case x :: xs if x == "max"                                                        => Right(Max(xs.map(_.toDouble)))

      case _ => Left(ErrorMessage("Something went wrong"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import com.sss.scalabootcamp.hometasks.task3.ControlStructuresHomework.Command._
    import com.sss.scalabootcamp.hometasks.task3.ControlStructuresHomework.Result._
    x match {
      case Divide(dividend, divisor) => (dividend, divisor) match {
        case (_, 0) => Left(ErrorMessage("Division by zero"))
        case _      => Right(DivideResult(dividend, divisor, dividend / divisor))
      }
      case Sum(numbers)              => numbers match {
        case Nil => Left(ErrorMessage("Haven't elements for summarize"))
        case _   => Right(SumResult(numbers, numbers.sum))
      }
      case Average(numbers)          => numbers match {
        case Nil => Left(ErrorMessage("Haven't elements to average"))
        case _   => Right(AverageResult(numbers, numbers.sum / numbers.length))
      }
      case Min(numbers)              => numbers match {
        case Nil => Left(ErrorMessage("Haven't elements to average"))
        case _   => Right(MinResult(numbers, numbers.min))
      }
      case Max(numbers)              => numbers match {
        case Nil => Left(ErrorMessage("Haven't elements to average"))
        case _   => Right(MaxResult(numbers, numbers.max))
      }

      case _ => Left(ErrorMessage("Something went wrong"))
    }
  }

  def renderResult(x: Result): String = {
    import com.sss.scalabootcamp.hometasks.task3.ControlStructuresHomework.Result._
    x match {
      case DivideResult(dividend, divisor, value) => s"$dividend divided by $divisor is $value"
      case SumResult(numbers, value)              => s"the sum of ${numbers.mkString(" ")} is $value"
      case AverageResult(numbers, value)          => s"the average of ${numbers.mkString(" ")} is $value"
      case MinResult(numbers, value)              => s"the min of ${numbers.mkString(" ")} is $value"
      case MaxResult(numbers, value)              => s"the max of ${numbers.mkString(" ")} is $value"
    }
  }

  def process(x: String): String = {
    //    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    // implement using a for-comprehension
    val result = for {
      c <- parseCommand(x)
      r <- calculate(c)
    } yield r

    result.fold(
      left => s"Error: " + left.value,
      right => renderResult(right)
    )
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
