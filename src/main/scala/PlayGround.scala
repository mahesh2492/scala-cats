import cats.Eval

object PlayGround extends App {
    val meaningOfLife = Eval.later {
      println("Learning Cats: computing abstractions and the meaning of life...")
      42
    }

    println(meaningOfLife.value)
}
