package part2abstractmath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers = (1 to 100).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  //General API
  /*def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  list.foldLeft(/* What ?*/)(_ |+| _)*/

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)
  val zero = intMonoid.empty //0

  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "Monoids")

  val emptyOption = Monoid[Option[Int]].empty
  val combineOptions = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])
  val combineOptions2 = Monoid[Option[Int]].combine(Option(2), Option(4)) // 6

  //extension method for monoids - |+|
  val combineOptionsFancy = Option(3) |+| Option(4)

  //Todo 1 : implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  //Todo 2 : combine a list of phonebooks as Maps[String, Int]
  val phonebooks: List[Map[String, Int]] = List(
    Map("Alice" -> 235, "Bob" -> 647),
    Map("Charlie" -> 372, "Daniel" -> 889),
    Map("Tina" -> 420)
  )

  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](ShoppingCart(List.empty[String], 0),
    (x: ShoppingCart, y: ShoppingCart) => ShoppingCart(x.items ++ y.items, x.total + y.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    println(combineFold(numbers))
    println(combineFold(List("Hello. ", "I am learning ", "Cats.")))
    println(combineFold(phonebooks))

    println(combineFold(List(
      ShoppingCart(List("shoes", "shocks"), 100),
      ShoppingCart(List("t-shirt", "shirt"), 1200),
      ShoppingCart(List("Jeans", "Shorts"), 1500),
      ShoppingCart(List(), 0)
    )))
  }
}
