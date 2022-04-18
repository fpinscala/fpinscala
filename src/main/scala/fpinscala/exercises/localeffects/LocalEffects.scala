package fpinscala.exercises.localeffects

import scala.reflect.ClassTag

import fpinscala.answers.monads.*

object Mutable:
  def quicksort(xs: List[Int]): List[Int] =
    if xs.isEmpty then xs else
      val arr = xs.toArray
      def swap(x: Int, y: Int) =
        val tmp = arr(x)
        arr(x) = arr(y)
        arr(y) = tmp

      def partition(l: Int, r: Int, pivot: Int) =
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = l
        for i <- l until r if arr(i) < pivotVal do
          swap(i, j)
          j += 1
        swap(j, r)
        j

      def qs(l: Int, r: Int): Unit =
        if l < r then
          val pi = partition(l, r, l + (r - l) / 2)
          qs(l, pi - 1)
          qs(pi + 1, r)

      qs(0, arr.length - 1)
      arr.toList

opaque type ST[S, A] = S => (A, S)
object ST:
  extension [S, A](self: ST[S, A])
    def map[B](f: A => B): ST[S, B] =
      s =>
        val (a, s1) = self(s)
        (f(a), s1)

    def flatMap[B](f: A => ST[S, B]): ST[S, B] =
      s =>
        val (a, s1) = self(s)
        f(a)(s1)

  def apply[S, A](a: => A): ST[S, A] =
    lazy val memo = a
    s => (memo, s)

  def lift[S, A](f: S => (A, S)): ST[S, A] = f

  def run[A](st: [s] => () => ST[s, A]): A =
    val su: ST[Unit, A] = st[Unit]()
    su(())(0)

final class STRef[S, A] private (private var cell: A):
  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S, Unit] = ST.lift[S, Unit] {
    s =>
      cell = a
      ((), s)
  }

object STRef:
  def apply[S, A](a: A): ST[S, STRef[S,A]] =
    ST(new STRef[S, A](a))

final class STArray[S, A] private (private var value: Array[A]):

  def size: ST[S, Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S, Unit] = ST.lift[S, Unit] {
    s =>
      value(i) = a
      ((), s)
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S, A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S, List[A]] = ST(value.toList)

  // Exercise 14.1
  def fill(xs: Map[Int, A]): ST[S, Unit] =
    ???

  def swap(i: Int, j: Int): ST[S, Unit] =
    for
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    yield ()

object STArray:
  // Construct an array of the given size filled with the value v
  def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A](Array.fill(sz)(v)))

  def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A](xs.toArray))

object Immutable:
  // Exercise 14.2
  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] =
    ???

  // Exercise 14.2
  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] =
    ???

  def quicksort(xs: List[Int]): List[Int] =
    if xs.isEmpty then xs else ST.run([s] => () =>
      for
        arr    <- STArray.fromList[s, Int](xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      yield sorted
   )
