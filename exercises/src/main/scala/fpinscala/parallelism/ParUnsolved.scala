package fpinscala.parallelism

sealed trait ParUnsolved[+A]

object ParUnsolved {
  def unit[A](a: => A): ParUnsolved[A] = ???

  def get[A](a: ParUnsolved[A]): A = ???

  /**
    * Exercise 7.1 - `Par.map2` is a new higher-order function for combining the result of two parallel computations.
    * What is its signature? Give the most general signature possible.
    */
  def map2[A, B, C](a: ParUnsolved[A], b: ParUnsolved[B])(f: (A, B) => C): ParUnsolved[C] = ???
}
