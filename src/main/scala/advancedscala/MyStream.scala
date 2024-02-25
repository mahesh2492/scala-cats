package advancedscala

import scala.annotation.tailrec

abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B]  // prepend operator
    def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate two streams

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A] // takes the first n elements out of this stream

    @tailrec
    final def toList[B >: A](acc: List[B] = Nil): List[B] = {
         if(isEmpty) acc.reverse
         else tail.toList(head :: acc)
    }
}
  object EmptyStream extends MyStream[Nothing] {
    override def isEmpty: Boolean = true
    override def head: Nothing = throw new NoSuchElementException
    override def tail: MyStream[Nothing] = throw new NoSuchElementException
    override def #::[B >: Nothing](element: B): MyStream[B] = new Cons[B](element, this)
    override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream
    override def foreach(f: Nothing => Unit): Unit = ()
    override def map[B](f: Nothing => B): MyStream[B] = this
    override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
    override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this
    override def take(n: Int): MyStream[Nothing] = this
  }

  class Cons[A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = false
    override val head: A = hd
    override lazy val tail: MyStream[A] = tl //call by need

    override def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)
    override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons[B](head, tail ++ anotherStream)

    override def foreach(f: A => Unit): Unit =  {
      f(head)
      tail.foreach(f)
    }

    /*
        val s = new advancedscala.Cons(1. ?)
        mapped = s.map(_ + 1) = new advancedscala.Cons(2, tail.map(_ + 1))
     */
    override def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))
    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
    override def filter(predicate: A => Boolean): MyStream[A] = {
      if(predicate(head)) {
          new Cons(head, tail.filter(predicate))
      } else {
          tail.filter(predicate) // preserver lazy eval
      }
    }

    override def take(n: Int): MyStream[A] = {
      if(n <= 0) {
        EmptyStream
      } else if(n == 1) {
        new Cons(head, EmptyStream)
      } else {
        new Cons(head, tail.take(n - 1))
      }
    }

  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] =
      new Cons[A](start, MyStream.from(generator(start))(generator))
  }

  object StreamPlayGround extends App {
    val naturals = MyStream.from(1)(_ + 1)
    println(naturals.head)
    println(naturals.tail.head)
    println(naturals.tail.tail.head)

    val startFrom0 = 0 #:: naturals // naturals.#::(0)

    println(startFrom0.head)
    startFrom0.take(100).foreach(println)

    println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1,  EmptyStream))).take(10).toList())
    println(startFrom0.filter(_ % 2 == 0).take(10).toList())

    def fibonacci(first: Int, second: Int): MyStream[Int] =
      new Cons(first, fibonacci(second, first + second))

    println(fibonacci(0, 1).take(100).toList())

    //eratosthenes sieve
    def eratosthenes(numbers: MyStream[Int]): MyStream[Int] =
      if(numbers.isEmpty) numbers
      else new Cons(numbers.head, eratosthenes(numbers.tail.filter(_ % numbers.head != 0)))

    println(eratosthenes(MyStream.from(2)(_ + 1)).take(20).toList())
  }