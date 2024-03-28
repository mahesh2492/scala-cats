package part3datamanipulation

import scala.util.Try

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("something error") // "left" value
  val aTest = Validated.cond(42 > 39, 99, "meaning of life is too small")

  //Todo: use Either
  /*
      - n must be prime
      - n must be non-negative
      - n <= 100
      - n must be even
   */
  val eitherToValidate = Validated.fromEither(Right(42))
  val optionToValidate = Validated.fromOption(None, List("nothing present here"))
  val tryToValidate = Validated.fromTry(Try("something".toInt))

  def isNonNegative(n: Int) = {
    Either.cond(n >= 0, true, Left("Not Non-Negative"))
  }

  /*def testNumber(n: Int): Either[List[String], Int] = {
     val isNotEven = if(n % 2 == 0) List() else List("Not Even")
     val isNonNegative = if(n >= 0) List() else List("Not Non-Negative")
     val isLessThan100 = if(n < 100) List() else List("Not Less than 100")
     val isNotPrime = if(isPrime(n)) List() else List("Not Prime")

     if(n % 2 == 0 && n >= 0 && n < 100 && isPrime(n)) Right(n)
     else
       Left(isNotEven ++ isNonNegative ++ isLessThan100 ++ isNotPrime)
  }*/

  def isLessThan100(n: Int) = {
    Either.cond(n < 100, true, Left("Not Less than 100"))
  }

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  //test valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)

  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  def isEven(n: Int): Either[Left[String, Nothing], Boolean] = {
    Either.cond(n % 2 == 0, true, Left("Not Even"))
  }

  def testNumber(n: Int): Validated[List[String], Int] = {
    Validated.cond(n % 2 == 0, n, List("Not Even"))
      .combine(Validated.cond(n >= 0, n, List("Not Non-Negative")))
      .combine(Validated.cond(n < 100, n, List("Not Less than 100")))
      .combine(Validated.cond(isPrime(n), n, List("Not Prime")))
  }

  def isPrime(n: Int) = {
    n > 1 & !(2 to n / 2).exists(p => n % p == 0)
  }

  aValidValue.toOption
  aValidValue.toEither

  //Todo 2: form validation exercise
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): Validated[List[String], String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): Validated[List[String], String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank."))

    def isEmailValid(email: String): Validated[List[String], String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid."))

    def passwordCheck(password: String): Validated[List[String], String] =
      Validated.cond(password.length >= 10, password, List("Password must be atleast 10 characters long"))
    /*
    fields are name, email, password

    Rules are -
    name, email, password must be there
    email should have '@'
    password must have >= 10 characters
    */

    def validateForm(form: Map[String, String]): FormValidation[String] = {
      getValue(form, "name").andThen(fieldName => nonBlank(fieldName, "name"))
        .combine(getValue(form, "email").andThen(email => isEmailValid(email)))
        .combine(getValue(form, "password").andThen(pass => passwordCheck(pass)))
        .map(_ => "User registration complete.")
    }

  }

  import cats.syntax.validated._

  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int]

  val form = Map(
    "name" -> "",
    "email" -> "mahesh.chandgmail.com",
    "password" -> "1267890"
  )

  def main(args: Array[String]): Unit = {
    //println(testNumber(115))
    println(FormValidation.validateForm(form))

  }

}

