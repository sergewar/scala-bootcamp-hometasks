package com.sss.scalabootcamp.hometasks.task7

import java.text.SimpleDateFormat
import java.util.Date

import cats.data.ValidatedNec

import scala.util.Try

object ErrorHandling {
  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
  object Homework {

    case class PaymentCard(
                            name: String,
                            cardNumber: String,
                            expDate: Date,
                            cvv: String)

    sealed trait ValidationError
    object ValidationError {
      final case object SymbolsInNameError extends ValidationError
      final case object ExpirationDateError extends ValidationError
      final case object CardNumbersError extends ValidationError
      final case object SecurityCodeError extends ValidationError
      final case object DateFormatError extends ValidationError
      final case object CreditCardExpiredError extends ValidationError
    }

    object PaymentCardValidator {
      import ValidationError._
      import cats.implicits._
      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      private def validateName(name: String): AllErrorsOr[String] = {
        if (name.matches("^[a-zA-Z]+$")) name.validNec
        else SymbolsInNameError.invalidNec
      }

      private def validateCardNumber(number: String): AllErrorsOr[String] = {
        if (number.matches("[0-9]{16}")) number.validNec
        else CardNumbersError.invalidNec
      }

      private def validateExpirationDate(date: String): AllErrorsOr[Date] = {
        def validateIsDate: AllErrorsOr[Date] = {
          Try(new SimpleDateFormat("dd.MM.yyyy").parse(date)).toOption match {
            case Some(convertedDate) => convertedDate.validNec
            case None =>
              Try(
                new SimpleDateFormat("dd/MM/yyyy").parse(date)
              ).toOption match {
                case Some(convertedDate) => convertedDate.validNec
                case None                => DateFormatError.invalidNec
              }
          }
        }

        def validateIsNotExpired(date: Date): AllErrorsOr[Date] = {
          if (date.before(new Date())) CreditCardExpiredError.invalidNec
          else date.validNec
        }

        validateIsDate andThen validateIsNotExpired
      }

      private def validateSecurityCode(cvv: String): AllErrorsOr[String] = {
        if (cvv.matches("[0-9]{3}")) cvv.validNec
        else CardNumbersError.invalidNec
      }

      def validate(
          name: String,
          number: String,
          expirationDate: String,
          securityCode: String
      ): AllErrorsOr[PaymentCard] =
        (
          validateName(name),
          validateCardNumber(number),
          validateExpirationDate(expirationDate),
          validateSecurityCode(securityCode)
        ).mapN(PaymentCard)
    }
  }
}
