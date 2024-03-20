package part2abstractmath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad
  object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(v)) => tailRecM(v)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  //Todo 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => value
      }
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def leaf[A](value: A): Tree[A] = Leaf(value)
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  }
  //Todo 2: define a monad for this tree
  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Tree.leaf(x)
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(value) => f(value)
        case Branch(left, right) => Tree.branch(flatMap(left)(f), flatMap(right)(f))
      }
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(tree: Tree[Either[A, B]]): Tree[B] =
        tree match {
          case Leaf(Left(value)) => stackRec(f(value))
          case Leaf(Right(value)) => Leaf(value)
          case Branch(left, right) => Branch(stackRec(left), stackRec(right))
        }
      stackRec(f(a))
    }

  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(t => Branch(Leaf(t + 1), Leaf(t + 2)))

    println(changedTree)
  }
}
