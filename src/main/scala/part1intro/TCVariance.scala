package part1intro

object TCVariance extends App {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found. Can't do this because cat is invariant
  // So how to do ? Option(2) == Option.empty[Int]

  //variance
  class Animal
  class Cat extends Animal

  //covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] //Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  //contravariant type: subtyping is propagated backward to the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  //Rule of thumb: "Has a T" = covariant, "Acts on T" = contravariant

  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]) = println("Working")

  makeSound[Cat] //ok - TC Instance defined above
  makeSound[Animal] // ok - TC instance for Animal is also applicable to Cats
  //Rule 1 - Contravariant TCs can use the superclass instance if nothing is available strictly for that type
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "cats everywhere"
  }

  def organizeShow[T](implicit event: AnimalShow[T]) = event.show
  //Rule 2: covariante TC will always use more specific TC instance for that type
  // but may refuse the compile if general TC is also present

  //Rule 3: You can't have both benefits
  //Cats use invariant TCs
  println(organizeShow[Cat])
  // println(organizeShow[Animal]) // will not compile - ambiguous values
}