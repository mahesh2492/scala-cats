package part2abstractmath

import cats.Semigroup
import cats.instances.int._

object SemiGroups {

  //Semigroups combine elements of same type

  val naturalIntSemiGroup = Semigroup[Int]
  val intCombination = naturalIntSemiGroup.combine(2, 40)

  import cats.instances.string._

  val naturalString = Semigroup[String]
  val stringCombination = naturalString.combine("I love ", "Cats")

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemiGroup.combine)

  def reduceString(list: List[String]): String = list.reduce(naturalString.combine)

  //general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]) = list.reduce(semigroup.combine)

  //Exercise 1 - Support a new type
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup = Semigroup.instance[Expense] {
    case (e1, e2) => Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  //extension methods from semigroups

  import cats.syntax.semigroup._

  val anIntSum = 2 |+| 3
  val aString = "We like" |+| " semigroup"
  val aCombinedExpense = Expense(1, 99) |+| Expense(2, 1)

  //Exercise 2: Implement reduceThings2 using |+|
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("I'm ", "starting ", "to ", "like ", "semigroup")
    println(reduceString(strings))

    println(reduceThings(numbers))
    println(reduceThings(strings))

    import cats.instances.option._
    val numberOptions = numbers.map(Option(_))

    println(reduceThings(numberOptions))

    println(reduceThings(List(Expense(1, 99), Expense(2, 35), Expense(43, 10))))

    println(reduceThings2(numbers))
  }
}
