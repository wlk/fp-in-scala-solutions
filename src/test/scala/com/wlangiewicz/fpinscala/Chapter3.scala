package com.wlangiewicz.fpinscala

import com.wlangiewicz.fpinscala.datastructures._

class exercise_3_1 extends FpTest {
  "res" should "be ok" in {

    val x = List(1, 2, 3, 4, 5)

    val res = x match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + List.sum(t)
      case _                                     => 101
    }
    res shouldBe 3
  }
}

class exercise_3_2 extends FpTest {
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }
  "tail" should "return tail of a list" in {
    tail(Nil) shouldBe Nil
    tail(Cons(1, Nil)) shouldBe Nil
    tail(Cons(1, Cons(2, Nil))) shouldBe Cons(2, Nil)
  }
}

class exercise_3_3 extends FpTest {
  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(newHead, xs)
  }

  "setHead" should "update head of a list" in {
    setHead(Nil, 1) shouldBe Nil
    setHead(Cons(1, Nil), 2) shouldBe Cons(2, Nil)
    setHead(Cons(1, Cons(2, Nil)), 3) shouldBe Cons(3, Cons(2, Nil))
  }
}

class exercise_3_4 extends FpTest {
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil                 => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case done                => done
  }

  "drop" should "drop elements from a list" in {
    drop(Nil, 99) shouldBe Nil
    drop(Nil, 0) shouldBe Nil
    drop(Cons(1, Nil), 99) shouldBe Nil
    drop(Cons(1, Cons(2, Nil)), 99) shouldBe Nil
    drop(Cons(1, Cons(2, Nil)), 1) shouldBe Cons(2, Nil)
  }
}

class exercise_3_5 extends FpTest {
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case done               => done
  }

  "dropWhile" should "drop elements from a list" in {
    dropWhile(Nil, (x: Int) => x == 0) shouldBe Nil
    dropWhile(Nil, (x: Int) => x == 0) shouldBe Nil
    dropWhile(Cons(1, Nil), (x: Int) => x > 0) shouldBe Nil
    dropWhile(Cons(1, Cons(2, Nil)), (x: Int) => x > 0) shouldBe Nil
    dropWhile(Cons(1, Cons(33, Cons(0, Nil))), (x: Int) => x <= 1) shouldBe Cons(33, Cons(0, Nil))
  }
}

class exercise_3_6 extends FpTest {
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  "init" should "copy the list except last element" in {
    init(Nil) shouldBe Nil
    init(Cons(1, Nil)) shouldBe Nil
    init(Cons(1, Cons(2, Nil))) shouldBe Cons(1, Nil)
    init(Cons(1, Cons(33, Cons(0, Nil)))) shouldBe Cons(1, Cons(33, Nil))
  }

}

class exercise_3_7 extends FpTest {
  def product(ns: List[Double]): Double = List.foldRight(ns, 0.0)(_ * _)

  var foldCalls = 0 // using var to track how many times foldRightShortCircuit has been called

  def foldRightShortCircuit[A, B](as: List[A], z: B)(
      f: (A, B) => B)(shortCircuitTrigger: A, shortCircuitReturn: B): B = {
    foldCalls = foldCalls + 1
    as match {
      case Nil                                    => z
      case Cons(x, _) if x == shortCircuitTrigger => shortCircuitReturn
      case Cons(x, xs) => f(x, foldRightShortCircuit(xs, z)(f)(shortCircuitTrigger, shortCircuitReturn))
    }
  }

  def productShortCircuit(ns: List[Double]): Double =
    foldRightShortCircuit(ns, 1.0)((x, y) => x + y)(0.0, 0.0)

  "foldRightShortCircuit" should "short circuit fold right" in {
    productShortCircuit(Cons(0.0, Cons(2.0, Cons(3.0, Nil)))) shouldBe 0.0
    foldCalls shouldBe 1
  }
}

class exercise_3_8 extends FpTest {
  "foldRight" should "x" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
  }
}

class exercise_3_9 extends FpTest {
  def length[A](as: List[A]): Int =
    List.foldRight(as, 0)((_, y) => y + 1)

  "length" should "calculate list length" in {
    length(Nil) shouldBe 0
    length(Cons(2, Nil)) shouldBe 1
    length(Cons(2, Cons(99, Nil))) shouldBe 2
  }
}

class exercise_3_10 extends FpTest {
  "foldLeft" should "reverse the list" in {
    List.foldLeft(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(y, x)) shouldBe Cons(3, Cons(2, Cons(1, Nil)))
  }
}

class exercise_3_11 extends FpTest {
  "sum" should "sum elements" in {
    def sum(xs: List[Int]): Int = List.foldLeft(xs, 0)((b, a) => b + a)

    sum(Nil) shouldBe 0
    sum(Cons(1, Cons(33, Nil))) shouldBe 34
  }

  "product" should "compute product of the list" in {
    def product(xs: List[Int]): Int = xs match {
      case Nil => 0
      case _   => List.foldLeft(xs, 1)((b, a) => b * a)
    }

    product(Nil) shouldBe 0
    product(Cons(2, Cons(33, Nil))) shouldBe 66
  }

  "length" should "calculate length of the list" in {
    def length(xs: List[Int]): Int = List.foldLeft(xs, 0)((b, a) => b + 1)

    length(Nil) shouldBe 0
    length(Cons(2, Cons(33, Nil))) shouldBe 2
  }

}

class exercise_3_12 extends FpTest {
  "reverse" should "reverse the list" in {
    def reverse(xs: List[Int]): List[Int] = List.foldLeft(xs, Nil: List[Int])((x, y) => Cons(y, x))

    reverse(Nil) shouldBe Nil
    reverse(Cons(1, Cons(2, Cons(3, Nil)))) shouldBe Cons(3, Cons(2, Cons(1, Nil)))
  }
}

class exercise_3_13 extends FpTest {
  "foldLeft via foldRight" should "reverse the list" in {
    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      List.foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    foldLeftViaFoldRight(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(y, x)) shouldBe Cons(3, Cons(2, Cons(1, Nil)))
  }

  "foldRight via foldLeft" should "not modify the list" in {
    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      List.foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(x, y)) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
  }
}

class exercise_3_14 extends FpTest {
  "append" should "append element to the list" in {

    List.append(Nil)(1) shouldBe Cons(1, Nil)
    List.append(Cons(1, Cons(2, Nil)))(3) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
  }

  "appendList" should "append 2 lists together" in {

    List.appendList(Nil)(Nil) shouldBe Nil
    List.appendList(Cons(1, Cons(2, Nil)))(Cons(3, Cons(4, Nil))) shouldBe Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  }
}

class exercise_3_15 extends FpTest {
  "concat" should "concatenate the lists" in {
    def concat(as: List[List[Int]]): List[Int] =
      List.foldRight(as, Nil: List[Int])(List.appendList(_)(_))

    val l1 = Cons(1, Cons(2, Nil))
    val l2 = Cons(3, Cons(4, Nil))

    concat(Cons(Nil, Cons(Nil, Nil))) shouldBe Nil
    concat(Cons(l1, Cons(l2, Nil))) shouldBe Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  }
}
