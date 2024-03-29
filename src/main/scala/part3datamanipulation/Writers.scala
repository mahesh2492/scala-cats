package part3datamanipulation

import cats.data.Writer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Writers {

  import cats.data.Writer
  //1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  //2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, log status the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "Found Something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "Found Something interesting", _ + 1) //both value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth{ (logs, value) =>
    (logs :+ "Found Something interesting", value + 1)
  }

  val writerA = Writer(Vector("Logs A1", "Logs A2"), 10)
  val writerB = Writer(Vector("Logs B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  //reset the logs
  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  //3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  //Todo 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if(n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if(n <= 0) Writer(Vector("starting"), 0)
    else {
      countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
    }
  }

  //Benefit 1: we work with pure FP

  //Todo 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if(n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed Sum(${n-1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveSumAndLog(n: Int): Writer[Vector[String], Int] = {
    if(n <= 0) Writer(Vector(), 0)
    else {
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- naiveSumAndLog(n - 1)
        _ <- Writer(Vector(s"Computed Sum(${n-1}) = $lowerSum"), n)
      } yield lowerSum + n
    }
  }

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(anEmptyWriter)
    countAndLog(5).written.foreach(println)

 //   println(naiveSum(10))
   // naiveSumAndLog(10).written.foreach(println)

  //  Future(naiveSum(100)).foreach(println)
  //  Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(naiveSumAndLog(10))
    val sumFuture2 = Future(naiveSumAndLog(10))

    val log1 = sumFuture1.map(_.written) // logs from thread 1
    log1.foreach(println)

    Thread.sleep(200)
  }
}
