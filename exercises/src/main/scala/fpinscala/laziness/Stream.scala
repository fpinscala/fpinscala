package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (n <= 0) empty else cons(h(), t().take(n - 1))
  }

  def take2(n: Int): Stream[A] = unfold((this, n))({ case (s, i) =>
    s match {
      case _ if i <= 0 => None
      case Empty => None
      case Cons(h, t) => Some((h(), (t(), i - 1)))
    }
  })

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(_, t) => if (n <= 1) t() else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, z) => if (p(h)) cons(h, z) else empty)

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this)({
      case Empty => None
      case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    })

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, z) => p(h) && z)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((h, z) => cons(f(h), z))

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, z) => if (p(h)) cons(h, z) else z)

  def append[A1 >: A](another: => Stream[A1]): Stream[A1] =
    foldRight[Stream[A1]](another)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((h, z) => f(h).append(z))

  def zipWith[B, C](another: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, another))({
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    })

  def zipAll[B](another: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, another))({
      case (Empty, Empty) => None
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    })

  def startsWith[B >: A](prefix: Stream[B]): Boolean =
    zipAll(prefix)
      .takeWhile({ case (_, ch2) => ch2.isDefined })
      .forAll({ case (ch1, ch2) => ch1 == ch2 })

  def tails: Stream[Stream[A]] =
    unfold(this)({
      case Empty => None
      case Cons(h, t) => Some((Cons(h, t), t()))
    }) append empty

  def hasSubsequence[B](sub: Stream[B]): Boolean =
    tails exists (_.startsWith(sub))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight[Stream[B]](empty)((h, b) => b match {
      case acc: Cons[B] => cons(f(h, acc.h()), acc)
      case Empty => cons(f(h, z), cons(z, empty))
    })

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constants[A](a: A): Stream[A] = Stream.cons(a, constants(a))

  val fibs: Stream[Int] = {
    def fibs2(prev: Int, cur: Int): Stream[Int] = Stream.cons(prev, fibs2(cur, prev + cur))

    fibs2(0, 1)
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((v, z1)) => cons(v, unfold(z1)(f))
  }

  val ones2: Stream[Int] = unfold(1)(n => Some((n, n)))

  def constants2[A](a: A): Stream[A] = unfold(a)(n => Some((n, n)))

  val fibs2: Stream[Int] = unfold((0, 1))({ case (prev, curr) => Some((prev, (curr, prev + curr))) })

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def main(args: Array[String]): Unit = {
    println()
    println("toList ->")
    val s1 = Stream(1, 2, 3, 4, 5)
    println(s1.toList)

    println()
    println("take ->")
    val s2 = Stream(1, 2, 3, 4, 5)
    println(s2.take(3).toList)

    println()
    println("take2 ->")
    val s22 = Stream(1, 2, 3, 4, 5)
    println(s22.take2(3).toList)

    println()
    println("drop ->")
    val s3 = Stream(1, 2, 3, 4, 5)
    println(s3.drop(3).toList)

    println()
    println("takeWhile ->")
    val s4 = Stream(1, 2, 3, 4, 5)
    println(s4.takeWhile(_ < 3).toList)

    println()
    println("takeWhile2 ->")
    val s42 = Stream(1, 2, 3, 4, 5)
    println(s42.takeWhile2(_ < 3).toList)

    println()
    println("takeWhile3 ->")
    val s43 = Stream(1, 2, 3, 4, 5)
    println(s43.takeWhile3(_ < 3).toList)

    println()
    println("exists ->")
    val s5 = Stream(1, 2, 3, 4, 5)
    println(s5.exists(_ > 2))

    println()
    println("forAll ->")
    val s6 = Stream(1, 2, 3, 4, 5)
    println(s6.forAll(_ < 3))

    println()
    println("headOption ->")
    val s7 = Stream()
    println(s7.headOption)

    println()
    println("map ->")
    val s8 = Stream(1, 2, 3, 4, 5)
    println(s8.map(_ * 2).take(3).toList)

    println()
    println("map2 ->")
    val s82 = Stream(1, 2, 3, 4, 5)
    println(s82.map2(_ * 2).take(3).toList)

    println()
    println("filter ->")
    val s9 = Stream(1, 2, 3, 4, 5)
    println(s9.filter(_ % 2 == 0).take(1).toList)

    println()
    println("append ->")
    val s11 = Stream(1, 2, 3, 4, 5)
    println(s11.append(Stream(6, 7, 8)).toList)

    println()
    println("flatMap ->")
    val s10 = Stream(1, 2, 3, 4, 5)
    println(s10.flatMap(n => Stream(n, n, n)).take(8).toList)

    println()
    println("zipWith ->")
    val s13 = Stream(1, 2, 3, 4, 5)
    val s14 = Stream(6, 7, 8)
    println(s13.zipWith(s14)(_ * _).toList)

    println()
    println("zipAll ->")
    val s132 = Stream(1, 2, 3, 4, 5)
    val s142 = Stream(6, 7, 8)
    println(s132.zipAll(s142).toList)

    println()
    println(ones.map(_ * 3).filter(_ < 15).take(3).toList)

    println()
    println(ones.take(3).toList)
    println(ones2.take(3).toList)
    println()
    println(constants("yo").take(5).toList)
    println(constants2("yo").take(5).toList)
    println()
    println(from(14).take(5).toList)
    println(from2(14).take(5).toList)
    println()
    println(fibs.take(7).toList)
    println(fibs2.take(7).toList)
    println()
    println(unfold(0)(z => Some((z, z + 1))).take(7).toList)


    println()
    println("startsWith ->")
    val s211 = Stream(1, 2, 3, 4, 5)
    val s222 = Stream(1, 2, 3, 4)
    println(s211.startsWith(s222))

    println()
    println("tails ->")
    val s311 = Stream(1, 2, 3, 4, 5)
    println(s311.tails.map(_.toList).toList)

    println()
    println("hasSubsequence ->")
    val s2111 = Stream(1, 2, 3, 4, 5)
    val s2222 = Stream(1, 2, 3, 4)
    println(s2111.hasSubsequence(s2222))


    println()
    println("scanRight ->")
    val s89 = Stream(1, 2, 3)
    println(s89.scanRight(0)(_ + _).toList)
  }
}