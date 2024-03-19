package part2abstractmath

import scala.util.Try

object Functors {

   val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
   val aModifiedOption = Option(2).map(_ + 1) // Option(3)
   val aModifiedTry = Try(42).map(_ + 1) // Try(43)

   trait MyFunctor[F[_]] {
     def map[A, B](initialValue: F[A])(f: A => B): F[B]
   }

  import cats.Functor
  import cats.instances.list._
  val listFunctor= Functor[List]
  val incrementalNumber = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._
  val optionFunctor = Functor[Option]
  val incrementalOption = optionFunctor.map(Option(2))(_ + 1)

  import cats.instances.try_._
  val incrementalTry = Functor[Try].map(Try(42))(_ + 1)

  //generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(list: Option[Int]): Option[Int] = list.map(_ * 10)
  def do10xTry(list: Try[Int]): Try[Int] = list.map(_ * 10)

  //generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  //Todo 1: define a functor for binary tree
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
      Branch(value, left, right)
  }

  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](initialValue: Tree[A])(f: A => B): Tree[B] =
      initialValue match {
        case Leaf(value) => Tree.leaf(f(value))
        case Branch(value, left, right) => Tree.branch(f(value), map(left)(f), map(right)(f))
      }
  }

  //extension method - map
  import cats.syntax.functor._
  val tree2 = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))

  def do10xShorter[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = container.map(_ * 10)
  //def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10) - same as above and called context bound

  def main(args: Array[String]): Unit = {
     println(do10x(List(1, 2, 3)))
     println(do10x(Option(2)))
     println(do10x(Try(3)))

    val tree = Tree.branch(1,
      Tree.branch(2,
        Tree.branch(3, Tree.leaf(4), Tree.leaf(5)),
        Tree.leaf(6)
      ),
      Tree.leaf(7)
    )

    println(do10x(tree))

    println(tree2.map(_ * 2))

    println(do10xShorter(List(1, 2, 3, 4)))
  }
}
