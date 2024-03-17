package part1recap

object Implicits extends App {
  case class Person(name: String) {
    def greet = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet = s"Hi, my name is $name"
  }

  val greeting: String = "Peter".greet  // new ImpersonableString("Peter").greet

  //implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount

  implicit val defaultAmount: Int = 10

  val increment2: Int = increment(5) //implicit argument 10 is passed by the compiler

  def multiply(x: Int)(implicit times: Int): Int = x * times
   val times: Int = multiply(2)

  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](value: List[T])(implicit serializer: JsonSerializer[T]): String =
    value.map( e => serializer.toJson(e)).mkString("[", ",","]")

  implicit val personSerializer = new JsonSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{ "name" : "${value.name}" }
         |""".stripMargin.trim
  }

  val personJson = listToJson(List(Person("Mahesh"), Person("Shivangi")))

  println(personJson)

  //implicit method - it will work for any type of case class with one argument
  implicit def oneArgCaseClassSerializer[T <: Product]: JsonSerializer[T] = new JsonSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}" : ${value.productElement(0)}}
         |""".stripMargin.trim
  }

  case class Cat(name: String)

  val catsToJson = listToJson(List(Cat("Tom"), Cat("Kity")))
  // in the background: val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // can be used for implicit conversions (DISCOURAGED)
  println(catsToJson)
}

