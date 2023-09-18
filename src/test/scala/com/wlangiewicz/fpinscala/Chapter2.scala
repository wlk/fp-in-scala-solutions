package com.wlangiewicz.fpinscala

class exercise_2_1 extends FpTest {
  def fib(n: Int): Int =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

  def fibTailrec(n: Int): Int = {
    @annotation.tailrec
    def go(steps: Int, prev: Int, next: Int): Int =
      if (steps == n)
        prev
      else
        go(
          steps = steps + 1,
          prev = next,
          next = next + prev
        )

    go(
      steps = 0,
      prev = 0,
      next = 1
    )
  }

  "fib" should "be ok" in {
    fib(0) shouldBe 0
    fib(1) shouldBe 1
    fib(2) shouldBe 1
    fib(3) shouldBe 2
    fib(4) shouldBe 3
    fib(5) shouldBe 5
    fib(6) shouldBe 8
    fib(7) shouldBe 13
  }

  "fibTailrec" should "be ok" in {
    fibTailrec(0) shouldBe 0
    fibTailrec(1) shouldBe 1
    fibTailrec(2) shouldBe 1
    fibTailrec(3) shouldBe 2
    fibTailrec(4) shouldBe 3
    fibTailrec(5) shouldBe 5
    fibTailrec(6) shouldBe 8
    fibTailrec(7) shouldBe 13
  }
}

class exercise_2_2 extends FpTest {
  @annotation.tailrec
  final def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    if (as.length >= 2)
      ordered(as(0), as(1)) && isSorted(as.drop(1), ordered)
    else
      true

  isSorted[Int](Array(1, 2, 3, 4, 5, 6), (a, b) => a >= b) shouldBe false
  isSorted[Int](Array(1), (a, b) => a <= b) shouldBe true
  isSorted[Int](Array(1, 2), (a, b) => a <= b) shouldBe true
  isSorted[Int](Array(1, 5, 3, 7, 2, 4, 9, 2), (a, b) => a >= b) shouldBe false
  isSorted[Int](Array(1, 2, 3, 4, 5, 6), (a, b) => a < b) shouldBe true
  isSorted[Int](Array.empty, (a, b) => a < b) shouldBe true
  isSorted[String](Array("a", "b", "c", "d"), (a, b) => a < b) shouldBe true
  isSorted[String](Array("a", "b", "c", "d"), (a, b) => a == b) shouldBe false

}

class exercise_2_3 extends FpTest {
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
}

class exercise_2_4 extends FpTest {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
}

class exercise_2_5 extends FpTest {
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}

