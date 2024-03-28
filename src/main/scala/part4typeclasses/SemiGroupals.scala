package part4typeclasses

import cats.Monad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object SemiGroupals {

  trait MySemiGroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]

  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some(123, "a string")
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._

  val aTupledFuture = Semigroupal[Future].product(Future(42), Future("the meaning of life")) // Future((42, "the meaning of life"))

  import cats.instances.list._

  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b")) // List((1, a), (1, b), (2, a), (2, b))

  //Todo - implement product with monads
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def product[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)


  //Monads extend Semigroupal
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroupal[List[_]]
  val invalidCombination = validatedSemigroupal.product(
    Validated.invalid(List("something went wrong", "something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  import cats.instances.either._
  type EitherErrorsOr[T] = Either[List[String], T]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product(
    Left(List("something went wrong", "something else wrong")),
    Left(List("This can't be right"))
  )

  //define a Semigroupal[List] which does a zip
  val semiGroupalList = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(invalidCombination)
    println(eitherCombination) // missed second error because implemented using map and flatmap - short circuiting
    println(semiGroupalList.product(List(1, 2), List("a", "b")))
  }

}