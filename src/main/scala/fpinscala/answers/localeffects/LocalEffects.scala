package fpinscala.answers.localeffects

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

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((k, v), st) => st.flatMap(_ => write(k, v))
    }

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
  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] =
    for
      vp <- a.read(pivot)
      _ <- a.swap(pivot, r)
      j <- STRef(l)
      _ <- (l until r).foldLeft(ST[S, Unit](()))((s, i) =>
        for
          _ <- s
          vi <- a.read(i)
          _  <-if vi < vp then
            for
              vj <- j.read
              _  <- a.swap(i, vj)
              _  <- j.write(vj + 1)
            yield ()
          else ST[S, Unit](())
        yield ())
      x <- j.read
      _ <- a.swap(x, r)
    yield x

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] =
    if l < r then for
      pi <- partition(a, l, r, l + (r - l) / 2)
      _ <- qs(a, l, pi - 1)
      _ <- qs(a, pi + 1, r)
    yield () else ST[S, Unit](())

  def quicksort(xs: List[Int]): List[Int] =
    if xs.isEmpty then xs else ST.run([s] => () =>
      for
        arr    <- STArray.fromList[s, Int](xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      yield sorted
   )

import scala.collection.mutable.HashMap

final class STMap[S, K, V] private (private val table: HashMap[K, V]):
  def size: ST[S,Int] = ST(table.size)

  // Get the value under a key
  def apply(k: K): ST[S,V] = ST(table(k))

  // Get the value under a key, or None if the key does not exist
  def get(k: K): ST[S, Option[V]] = ST(table.get(k))

  // Add a value under a key
  def +=(kv: (K, V)): ST[S,Unit] = ST(table += kv)

  // Remove a key
  def -=(k: K): ST[S,Unit] = ST(table -= k)

object STMap:
  def empty[S, K, V]: ST[S, STMap[S, K, V]] =
    ST(new STMap(HashMap.empty))

  def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] =
    ST(new STMap((HashMap.newBuilder ++= m).result()))
