package fpinscala.iomonad

trait Trampoline[+A] { def run: A = Trampoline.run(this) }

// companion object itself is the Monad instance
object Trampoline extends Monad[Trampoline] {

  case class Done[+A](get: A) extends Trampoline[A]
  case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
  case class Bind[A,+B](force: () => Trampoline[A], 
                        f: A => Trampoline[B]) extends Trampoline[B]
 
                            /* 

  Exercise 5: Implement a tail-recursive `run` function for evaluating
  a `Trampoline[A]` to an `A`.
  
  Exercise 6: Implement `Monad[Trampoline]`. 

                             */

  @annotation.tailrec
  def run[A](t: Trampoline[A]): A = t match {
    case Done(a) => a
    case More(k) => run(k())
    case Bind(force, f) => run(force() flatMap f)
  }

  def unit[A](a: => A) = Done(a)
  def flatMap[A,B](a: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = 
    a match { 
      case Done(forced) => f(forced)
      case More(force) => Bind(force, f)
      case Bind(force,g) => More(() => Bind(force, g andThen (_ flatMap f)))
    }
  def more[A](a: => Trampoline[A]): Trampoline[A] = 
    More(() => a)
  def delay[A](a: => A): Trampoline[A] = 
    More(() => Done(a))
  def done[A](a: A): Trampoline[A] = Done(a)
}
                            /* 

  Exercise 7: Show that `flatMap` always returns after doing a constant
  amount of work, and that `run` will always call itself after at most
  a single call to `force()`.
 
  First, we show that `flatMap` always does a constant amount of
  work before returning. Just by inspecting each of the cases, we
  can see that `flatMap` always returns 'immediately': 
    
    Bind(() => a, f) returns immediately, it is just packaging
      the arguments in a constructor
    Bind(force, f) returns immediately, for the same reason
    More(() => Bind(force, g andThen (_ flatMap f))) returns
      immediately. The `g andThen (_ flatMap f)` creates a new 
      function that could be problematic when called, but 
      composing the functions returns immediately.

  The next observation is that we never build up a chain of function
  compositions. Look at the third case: 

    case Bind(force,g) => More(() => Bind(force, g andThen (_ flatMap f)))

  Since this wraps its result in a `More`, if we call `flatMap` again,
  this will just become:

    Bind(() => Bind(force, g andThen (_ flatMap f)), f2)
  
  The same pattern continues if we flatMap again. Since we never 
  construct a function composition like f1 andThen f2 ... andThen fN, we
  don't have to worry about stack overflows when calling the function in 
  a Bind constructor.

  Next, notice that we never call force() anywhere in the implementation 
  of flatMap. The one place we construct a new thunk, it is a thunk that
  returns a Bind 'immediately', without forcing any other thunk:

     case Bind(force,g) => More(() => Bind(force, g andThen (_ flatMap f)))

  Since we never construct a thunk that forces another thunk, and any 
  thunk we do construct uses constant stack space to produce its result,
  we can be assured that calling `force()` in the `run` function will use
  a constant amount of stack space before returning. 

                             */
  
