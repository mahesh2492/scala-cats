package part4typeclasses

import cats.{Applicative, Foldable, Functor, Id, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod-rockthejvm.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  val allBandwidth: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  val allBandwidthsTraverse = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence = Future.sequence(servers.map(getBandwidth))

  //Todo 1
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def listTraverse[F[_]: Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement = func(element)
      for {
        acc <- wAccumulator
        element <- wElement
      } yield acc :+ element
    }
  }

  import cats.syntax.apply._
  def listTraverseApplicative[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement = func(element)

      (wAccumulator, wElement).mapN(_ :+ _)
    }
  }

  //Todo implement sequence
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverseApplicative(list)(identity)

  //Todo 3 - What will be output
  val allPairs = listSequence(List(Vector(1, 2), Vector(3, 4))) // List((1, 3), (1, 4), (2, 3), (2, 4))
  val allTriples = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

   def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
     listTraverse(list)(x => Some(x).filter(predicate))

  val allTrue = filterAsOption(List(2, 4, 6))(_ % 2 == 0)
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0)

  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list){ x =>
      if(predicate(x)) Validated.valid(x)
      else Validated.invalid(List(s"Predicate has failed for $x"))
    }

  val allTrueValidated = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2, 4, 6))
  val someFalseValidated = filterAsOption(List(1, 2, 3))(_ % 2 == 0) //Invalid(List("Predicate has failed for 1", "Predicate has failed for 3")

  import cats.data.Validated
  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
