package part4typeclasses

import cats.{Eval, Foldable, Monoid, catsParallelForId}
import cats.implicits.catsSyntaxSemigroup
import part4typeclasses.Folding.ListExercises.sumRight

object Folding {

  //Todo - implement all in terms of foldLeft
  object ListExercises {

    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B]) { case (a, currentList) => f(a) :: currentList }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B]) { case (currentList, a) => f(a) ++ currentList }

    def filter[A](list: List[A])(f: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, acc) => if (f(a)) a :: acc else acc )

    def combineAll[A, B](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty){ case (a, b) => a |+| b }

    import cats.Foldable
    import cats.instances.list._
    val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) //6

    import cats.instances.option._
    val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _)

    //foldRight is stack-safe regardless of your container
    val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2,3), Eval.now(0)) { (num, eval) =>
      eval.map(_ + num)
    }

    val anotherSum = Foldable[List].combineAll(List(1, 2, 3))
    val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString)

    val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
    import cats.instances.vector._
    import cats.instances.list._
    (Foldable[List].compose(Foldable[Vector]).combineAll(intsNested))

    import cats.syntax.foldable._
    val sum3 = List(1, 2, 3).combineAll // req Foldable[List], Monoid[Int]
    val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)
  }

  def main(args: Array[String]): Unit = {
    val list = (1 to 10).toList
    println(ListExercises.map(list)(_ * 2))
    println(ListExercises.flatMap(list)(f => List(f + 2)))
    println(ListExercises.filter(list)(_ * 2 == 0))

    println(sumRight.value)
  }
}
