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

// We can also write the loop using a left fold. When the loop has more than
// one piece of state like this (here we have the current state and the list
// of values we have accumulated so far), it can be a little awkward to have 
// to pack and unpack this state into tuples 
def sequenceViaFoldLeft[S, A](sas: List[State[S,A]]) = 
  State((s: S) => sas.foldLeft((List[A](),s)) { (t,action) => t match {
    case (acc,s) => 
      val (a,s2) = action.run(s)
      (a :: acc, s2)
  }} match { case (acc,s) => (acc.reverse,s) })
}