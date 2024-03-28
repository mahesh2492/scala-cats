package part4typeclasses

object Applicatives {
  //Applicatives = Functor + the pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._
  val optionApplicatives = Applicative[Option]
  val aOption = optionApplicatives.pure(4) // Some(3)

  //pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List]
  val aSweetOption = 2.pure[Option]

  //Monads extend Applicatives
  //Applicatives extend Functor
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue = Validated.valid(43) // like pure
  val aModifiedValidated = aValidValue.map( _ + 1)
  val validatedApplicative = Applicative[ErrorsOr]

  //Todo thought experiment
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    //applicative.product(wa, wb)
    val functionWrapper = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  //Applicatives have this ap method - ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B]
  //Applicatives can implement product from semigroupal
  // Applicatives extend Semigroupal

  def main(args: Array[String]): Unit = {
    println(productWithApplicative(Option(42), Option("Meaning of life")))
  }

}
