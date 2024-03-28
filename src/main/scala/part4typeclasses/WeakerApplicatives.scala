package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  trait MyApply [W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    //Todo
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }

    def ap[A, B](wf: W[A => B])(wa: W[A]): W[B]
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental
  }

  import cats.Apply
  import cats.instances.option._
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) //Some(2)

  import cats.syntax.apply._
  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOptions.tupled // Some((1, 2, 3))
  val sumOfOption = tupleOfOptions.mapN(_ + _ + _ ) // Some(6)


  def main(args: Array[String]): Unit = {

  }
}