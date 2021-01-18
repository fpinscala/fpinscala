package fpinscala.state

import State._
import fpinscala.state.RNG.Simple
import fpinscala.state.RNG._

// ch 6.5 a general state action data type
// state action, or state transition
// S is a type with some state, A is the output besides state change
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {

  // alias: RNG => (A, RNG)
  type Rand[A] = State[RNG, A]

  def main(args: Array[String]): Unit = {
    println("State.main()")

    val intRand: Rand[Int] = State[RNG, Int](_.nextInt)
    println("intRand:" + intRand) // State(fpinscala.state.State$$$Lambda$5/226170135@2ff4f00f)
    println("intRand.run:" + intRand.run) // fpinscala.state.State$$$Lambda$5/226170135@2ff4f00f
    println("intRand.run():" + intRand.run(Simple(42))) // (16159453,Simple(1059025964525))
    // same as defining the State directly
    val intState: State[RNG, Int] = State[RNG, Int](_.nextInt)
    println("intState:" + intState.run(Simple(42))) // (16159453,Simple(1059025964525)) // SAME
    println("eq:" + intState.eq(intRand)) // false


    /* not compiling
    val ns: Rand[List[Int]] =
      int.flatMap(x =>
        int.flatMap(y =>
          ints(x).map(xs =>
            xs.map(_ % y))))

    val ns: Rand[List[Int]] = for {
      x <- int
      y <- int
      xs <- ints(x)
    } yield xs.map(_ % y)
    */


    // println("intState.get:" + intState.get) // compile error - get is in companion object
    println("State.get:" + State.get) // State(fpinscala.state.State$$$Lambda$8/2137589296@edf4efb)
    println("get:" + get)             // State(fpinscala.state.State$$$Lambda$8/2137589296@edf4efb) // SAME
    println("get[]:" + get[RNG])      // State(fpinscala.state.State$$$Lambda$8/2137589296@edf4efb) // SAME
    println("get.getClass:" + get.getClass) // class fpinscala.state.State
    // println("get:" + get()) // error - get does not take parameters
    // println("get:" + get(1)) // error - get does not take parameters
    // println("get:" + get(intState)) // error - get does not take parameters
    // println("get[]():" + get[RNG]()) // error - xxx does not take parameters
    println("get.run:" + get.run)     // SAME lambda
    println("get.run():" + get.run(Simple(42))) // (Simple(42),Simple(42))
    println("get.run():" + get.run("anything")) // (anything,anything)
    // println("get.run():" + get.run(_ => ("b", "c"))) // (anything,anything)

    println("set:" + set(intState)) // State(fpinscala.state.State$$$Lambda$9/796533847@566776ad)
    // println("modify:" + modify(intState => intState)) // error - missing parameter type ???!!!
    println("modify:" + modify[RNG](intState => intState)) // State(fpinscala.state.State$$Lambda$12/1811044090@6cd8737)


    println("State.get.run:" + State.get.run) // fpinscala.state.State$$$Lambda$8/2137589296@edf4efb
    println("State.get.run():" + State.get.run(Simple(42))) // (Simple(42),Simple(42))
    println("State.get.run():" + State.get.run(intState)) // (State(fpinscala.state.State$$$Lambda$7/905544614@2f7a2457),State(fpinscala.state.State$$$Lambda$7/905544614@2f7a2457))

    //println("State.modify:" + modify(Simple(42) => intState.run(_))) // (State(fpinscala.state.State$$$Lambda$7/905544614@2f7a2457),State(fpinscala.state.State$$$Lambda$7/905544614@2f7a2457))
  }

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))


  // Unit means no return value except the state change
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  // ignore the incoming state
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify1[S](f: S => S): State[S, Unit] = State(s => ((), f(s)))

  /*
  def modify2[S](f: S => S): State[S, Unit] = for {
    currentState <- State(s => (s, s))
    newState <- State(currentState => ((), f(currentState)))
  } yield newState
  */
}
