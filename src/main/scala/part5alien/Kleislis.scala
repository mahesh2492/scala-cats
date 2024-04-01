package part5alien

object Kleislis {

    val func1: Int => Option[String] = x => if(x % 2 == 0) Some(s"$x is even") else None
    val func2: Int => Option[Int] = x => Some(x * 3)

    //val func3 = func1 andThen func2

  val plainFunc1: Int => String = x => if(x % 2 == 0) s"$x is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val func3: Int => String = plainFunc2 andThen plainFunc1
  import cats.data.Kleisli

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3k: Kleisli[Option, Int, String] = func2K andThen func1K

  //convenience
  val multiply = func2K.map(_ * 2) // x => Option(....).map(_ * 2)
  val chain = multiply.flatMap(x => func1K)

  //Todo
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // InterestingKleisli == Reader!

  val times2 = Kleisli[Id, Int, Int](x => x * 2)
  val plus4 = Kleisli[Id, Int, Int](x => x + 3)
  val composed = times2.flatMap(t2 => plus4.map(_ + t2))
  val composed4 = for {
    t <- times2
    p <- plus4
  } yield t + p

  def main(args: Array[String]): Unit = {
    println(composed(3))
  }
}
