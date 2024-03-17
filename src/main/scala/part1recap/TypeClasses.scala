package part1recap

object TypeClasses extends App {

  case class Person(name: String, age: Int)

  //part 1 - type class definition
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  //part 2 - create implicit type class Instances
  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = value
  }

  implicit object NumberSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = "\"" + value + "\""
  }

  implicit object JsArray extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
        |{ "name": "${person.name}", "age": "${person.age}"}
        |""".stripMargin.trim
  }

  //part 3 - offer some API
  def convertListToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  println(convertListToJson(List(Person("Alice", 23), Person("Bob", 35))))

  //part 4 - extending the existing types via extension methods
  object JsonSyntax {
    implicit class JsonSerializable[T](value: T)(implicit serializer: JsonSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  val xavier = Person("Xavier", 35)
  import JsonSyntax._
  println(xavier.toJson)
}
