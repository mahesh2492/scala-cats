package part2abstractmath

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Monads {

   //list
  val numberList = List(1, 2, 3)
  val charList = List('a', 'b', 'c')

  //Todo 1.1: how do u create all combinations of (number, char) ?
  val res = for {
    i <- numberList
    c <- charList
  } yield (i, c)

  //options
  var numberOption = Option(2)
  val charOption = Option('a')
  //Todo 1.2: how do u create the combination of (number, char) ?
  val optionRes = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  //future
  val numberFuture = Future(2)
  val charFuture = Future('a')
  //Todo 1.3:  how do u create the combination of (number, char) ?
  val futureRes = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
     Pattern
      - wrapping a value into a M value
      - the flatMap mechanism

      Monads
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  //Cats Monad
  import cats.Monad
  import cats.instances.option._
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformation = optionMonad.flatMap(anOption)(x => if(x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformationList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(3, 4)

  //Todo 2: use a Monad[Future]

  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(3) // requires an implicit execution context
  val aTransFormationFuture = futureMonad.flatMap(aFuture)(x => Future(x * 10))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  //generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  def main(args: Array[String]): Unit = {
      println(getPairs(numberList, charList))
      println(getPairs(numberOption, charOption))
      getPairs(numberFuture, charFuture).foreach(println)
  }
}
