package part3datamanipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, ten) = countAndSay.run(10).value
  // state = "iterative" computation

  //iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained ${a + 1}"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained ${a * 5}"

  // pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${a + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${a * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }

  val compositeTransformationFor = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  //Todo 1: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { shoppingCart =>
    (ShoppingCart(item :: shoppingCart.items, shoppingCart.total + price), price + shoppingCart.total)
  }

  val danielsCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Guitar", 1500)
    _ <- addToCart("Books", 700)
    total <- addToCart("Speakers", 1000)
  } yield total

  //Todo 2: pure mental gymnastics
  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State(s => (s, f(s)))

  // returns a State data structure that, when run, returns the value of that state and makes no change
  def get[A]: State[A, A] = State(s => (s, s))

  // returns a State data structure that, when run, returns unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  // returns a State data structure that, when run, returns unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(s => (f(s), ()))

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 3)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeTransformationFor.run(10).value)
    println(danielsCart.run(ShoppingCart(Nil, 0)).value)
    println(inspect((x : Int) => x * 10).run(2).value)
    println(get.run(2).value)
    println(set(5).run(12).value)
    println(modify((x: Int) => x * 3).run(12).value)

    println(program.run(2).value)
  }
}
