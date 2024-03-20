package part2abstractmath

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  //option transformer
  import cats.data.OptionT
  import cats.instances.list._

  val listOfOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    number <- listOfOptions
    char <- listOfCharOptions
  } yield (number, char)

  //either transformer
  import cats.data.EitherT
  import cats.instances.list._

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Something Wrong"), Right(43), Right(12)))
  val futureOfEithers = EitherT.right(Future(45))

  /* TODO exercise
  We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
  We measure bandwidth in units.
  We want to allocate TWO of our servers to cope with the traffic spike.
  We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
  */
  val bandwidth = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, Int]]

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidth.get(server) match {
      case Some(value) => EitherT.right(Future(value))
      case None => EitherT.left(Future(s"Server $server unreachable"))
    }

  //Todo 1
  def canWithStandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    } yield b1 + b2 > 250

  // Todo 2
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    canWithStandSurge(s1, s2)
      .transform {
        case Left(reason) => Left(s"Server $s1 and $s2 cannot cope up with the incoming spike: $reason")
        case Right(false) => Left(s"Server $s1 and $s2 cannot cope up with the incoming spike: not enough total bandwidth")
        case Right(true) => Right(s"Server $s1 and $s2 can cope up with the incoming spike: No Problem")
      }

  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    val resultFuture = generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value //.map(println)
    resultFuture.foreach(println)

    Thread.sleep(100)
  }
}
