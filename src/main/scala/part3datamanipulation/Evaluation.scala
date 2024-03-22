package part3datamanipulation

object Evaluation {

  /*
  Cats makes the distinction between
  - evaluating an expression eagerly
  - evaluating lazily and every time you request it
  - evaluating lazily and keeping the value (memoizing)
  */

  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now")
    123
  }

  val redoEval = Eval.always {
    println("Computing again!")
    2
  }

  val delayedEval = Eval.later {
    println("Computing later")
    4000
  }

  val composedEvaluation = instantEval.flatMap(value => delayedEval.map(v => v + value))
  val composedEvaluationFor = for {
    v <- instantEval
    value <- delayedEval
  } yield v + value

  //Todo 1: Predict the output
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  /*
  output - now, later, again, again, sum, again, again, sum
  */

  //remember a computed value
  val tutorial = Eval.
    always { println("step 1..."); "put the guitar on your lap"}
    .map { step1 => println("step 2.."); s"$step1 then put your left hand on the neck"}
    .memoize
    .map {step12 => println("step 3, more complicated"); s"$step12 then with the right hand strike the strings" }
  val dontRecompute = redoEval.memoize

  //Todo 2: implement defer such that defer(Eval.now) does not run the side effects.
  def defer[T](eval: => Eval[T]): Eval[T] = {
    Eval.later(eval).flatMap(identity)
  }

  //Todo 3: rewrite the method with evals
  def reverseList[T](list: List[T]): List[T] =
    if(list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if(list.isEmpty) Eval.now(list)
    else
      Eval.defer(reverseEval(list.tail).map(_ :+ list.head))
  }

  def main(args: Array[String]): Unit = {
    //println(instantEval.value)
    //println(redoEval.value)
    //println(redoEval.value)
    //println(delayedEval.value)
    //println(delayedEval.value)
    // println(composedEvaluation.value)
    // println(composedEvaluation.value)
    /*println(evalEx1.value)
    println(evalEx1.value)*/

    /*println(dontRecompute.value)
    println(dontRecompute.value)*/

    /* println(tutorial.value)
    println(tutorial.value)*/

    /* println(defer(Eval.now {
     println("Now!")
     42
     }).value)*/
  }

  println(reverseEval((1 to 10000).toList).value)
}