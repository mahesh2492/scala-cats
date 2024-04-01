package part4typeclasses

import cats.Monad

object ErrorHandling {

  trait MyMonadError[M[_], E] extends Monad[M] {
      def raiseError[A](e: E): M[A]
  }

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32) //Either[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("Something wrong") //Either[String, Int] == Left("Something wrong")

  //recover
  val handleError = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }

  //recoverWith
  val handleError2 = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44)
    case _ => Left("Something else")
  }

  //filter
  val filteredSuccess = monadErrorEither.ensure(success)("Number is too small")(_ > 100)

  def main(args: Array[String]): Unit = {
      println(filteredSuccess)
  }
}
