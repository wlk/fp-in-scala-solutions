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
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  "tail" should "return tail of a list" in {
    tail(Nil) shouldBe Nil
    tail(Cons(1, Nil)) shouldBe Nil
    tail(Cons(1, Cons(2, Nil))) shouldBe Cons(2,Nil)
  }
}

class exercise_3_3 extends FpTest {
  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Nil => Nil
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
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n-1)
    case done => done
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
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case done => done
  }

  "dropWhile" should "drop elements from a list" in {
    dropWhile(Nil, _: Nothing => true) shouldBe Nil
    dropWhile(Nil,  _: Nothing => false) shouldBe Nil
    dropWhile(Cons(1, Nil), _: Int => true) shouldBe Nil
    dropWhile(Cons(1, Cons(2, Nil)), _: Int => false) shouldBe Nil
    dropWhile(Cons(1, Cons(33, Cons(0, Nil))), (x: Int) => x <= 1) shouldBe Cons(33, Cons(0, Nil))
  }
}

class exercise_3_6 extends FpTest {
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  "init" should "copy the list except last element" in {
    init(Nil) shouldBe Nil
    init(Cons(1, Nil)) shouldBe Nil
    init(Cons(1, Cons(2, Nil))) shouldBe Cons(1, Nil)
    init(Cons(1, Cons(33, Cons(0, Nil)))) shouldBe Cons(1, Cons(33, Nil))
  }

  }