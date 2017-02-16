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
