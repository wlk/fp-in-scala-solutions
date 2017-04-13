package com.wlangiewicz.fpinscala

object exercise_2_1 extends App {
  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)
  }

  def fibTailrec(n: Int): Int = {
    @annotation.tailrec
    def go(steps: Int, prev: Int, next: Int): Int = {
      if (steps == n)
        prev
      else go(
        steps = steps + 1,
        prev = next,
        next = next + prev
      )
    }

    go(
      steps = 0,
      prev = 0,
      next = 1
    )
  }

  println(fib(0))
  println(fib(1))
  println(fib(2))
  println(fib(3))
  println(fib(4))
  println(fib(5))
  println(fib(6))
  println(fib(7))

  println(fibTailrec(0))
  println(fibTailrec(1))
  println(fibTailrec(2))
  println(fibTailrec(3))
  println(fibTailrec(4))
  println(fibTailrec(5))
  println(fibTailrec(6))
  println(fibTailrec(7))
}

object exercise_2_2 extends App {
  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length >= 2)
      ordered(as(0), as(1)) && isSorted(as.drop(1), ordered)
    else
      true

  }

  println(isSorted[Int](Array(1, 2, 3, 4, 5, 6), (a, b) => a >= b))
  println(isSorted[Int](Array(1), (a, b) => a <= b))
  println(isSorted[Int](Array(1,2), (a, b) => a <= b))
  println(isSorted[Int](Array(1, 5, 3, 7, 2, 4, 9, 2), (a, b) => a >= b))
  println(isSorted[Int](Array(1, 2, 3, 4, 5, 6), (a, b) => a < b))
  println(isSorted[Int](Array.empty, (a, b) => a < b))
  println(isSorted[String](Array("a", "b", "c", "d"), (a, b) => a < b))
  println(isSorted[String](Array("a", "b", "c", "d"), (a, b) => a == b))

}

object exercise_2_3 extends App {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
}

object exercise_2_4 extends App {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
}

object exercise_2_5 extends App {
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}