package fpinscala.streamingio

import fpinscala.iomonad.{Task,IO,Monad}

object ImperativeAndLazyIO {

                            /*                       
  
  We are going to consider various approaches to the simple task of 
  checking whether a file contains more than 40,000 lines.
  
  Our first implementation is an imperative implementation, embedded
  into `IO`. 
                             */

  type IO[A] = fpinscala.iomonad.IO[Task,A] // see `Task.scala` for details
  
  import java.io._

  def linesGt40k(filename: String): IO[Boolean] = IO {
    // There are a number of convenience functions in scala.io.Source
    // for reading from external sources such as files.
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      // Obtain a stateful iterator from the Source
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next // has side effect of advancing to next element 
        count += 1
      }
      count > 40000
    }
    finally src.close
  }

                            /*                       
  
  The above code is rather low-level, and it's not compositional,
  either. Consider the following scenarios: 

  * Check whether the number of _nonempty_ lines in the file exceeds
    40,000
  * Find a line index before 40,000 where the first letter of 
    consecutive lines spells out `"abracadabra"`.
  
  We cannot just compose our existing implementation with some
  other combinator(s) to implement these tasks. Our implementation is
  a monolithic loop, and we must modify this loop directly if we want
  to change its behavior.

  Now imagine if we had a `Stream[String]` for the lines of the file
  and we could assemble functionality using all the `Stream` functions
  we know and love. 
                             */

  object Examples {
    val lines: Stream[String] = sys.error("defined elsewhere") 
    val ex1 = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)
  }
  
                            /*                       
  
  Could we actually write the above? Not quite. We could 'cheat' and
  return an `IO[Stream[String]]` representing the lines of a file:

                             */

  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; Stream.empty }
  }
                            /*                       
  
  This is called _lazy I/O_, and it's problematic for a number of 
  reasons, discussed in the book text. However, it would be nice to
  recover the same high-level, compositional style we are used to
  from our use of `List` and `Stream`.

                             */
}

object SimpleStreamTransducers {
  
  type IO[A] = fpinscala.iomonad.IO[Task,A]

                            /*                       
  
  We now introduce a type, `Process`, representing pure, single-input
  stream transducers. It can be in of three states - it can be
  emitting a value to the output (`Emit`), reading a value from its 
  input (`Await`) or signaling termination via `Halt`. 

                             */

  sealed trait Process[I,O] {
    import Process._
    
    /* 
     * A `Process[I,O]` can be used to transform a `Stream[I]` to a
     * `Stream[O]`.  
     */
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv, fallback) => s match {
        case h #:: t => recv(h)(t) 
        case _ => fallback(s) // Stream is empty
      }
      case Emit(h,t) => h.toStream append t(s)
    }

    /*
     * `Process` can be thought of as a sequence of values of type `O`
     * and many of the operations that would be defined for `List[O]`
     * can be defined for `Process[I,O]`, for instance `map`, `++` and
     * `flatMap`. The definitions are analogous.
     */ 

    def map[O2](f: O => O2): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(h map f, t map f)
      case Await(recv,fb) => Await(recv andThen (_ map f), fb map f)
    }
    def ++(p: => Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Emit(h, t) => emitAll(h, t ++ p) 
      case Await(recv,fb) => Await(recv andThen (_ ++ p), fb ++ p)
    }
    def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => 
        if (h.isEmpty) t flatMap f
        else f(h.head) ++ emitAll(h.tail, t).flatMap(f)
      case Await(recv,fb) => 
        Await(recv andThen (_ flatMap f), fb flatMap f)
    }

    /* 
     * Exercise 1: Implement `|>`. Let the types guide your implementation.
     */
    def |>[O2](p2: Process[O,O2]): Process[I,O2] = {
      // if this is emitting values and p2 is consuming values,
      // we feed p2 in a loop to avoid using stack space
      @annotation.tailrec
      def feed(emit: Seq[O], tail: Process[I,O], 
               f: O => Process[O,O2], fb: Process[O,O2]): Process[I,O2] = 
        if (emit isEmpty) tail |> Await(f, fb) 
        else f(emit.head) match {
          case Await(f2, fb2) => feed(emit.tail, tail, f2, fb2)
          case p => Emit(emit, tail) |> p
        }
      p2 match {
        case Halt() => Halt()
        case Emit(h,t) => Emit(h, this |> t)
        case Await(f,fb) => this match {
          case Emit(h,t) => feed(h, t, f, fb)
          case Halt() => Halt() |> fb
          case Await(g,gb) => Await((i: I) => g(i) |> p2, gb |> fb)
        }
      }
    }
    
    /* 
     * See `Process.lift` for a typical repeating `Process`
     * definition expressed with explicit recursion.
     */

    /* 
     * `Process` definitions can often be expressed without explicit 
     * recursion, by repeating some simpler `Process` forever. 
     */
    def repeat: Process[I,O] = {
      def go(p: Process[I,O]): Process[I,O] = p match {
        case Halt() => go(this)
        case Await(recv,fb) => Await(recv andThen go, fb)
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

    def repeatN(n: Int): Process[I,O] = {
      def go(n: Int, p: Process[I,O]): Process[I,O] = p match {
        case Halt() => if (n > 0) go(n-1, this) else Halt()
        case Await(recv,fb) => Await(recv andThen (go(n,_)), fb)
        case Emit(h, t) => Emit(h, go(n,t))
      }
      go(n, this) 
    }
    
    /* 
     * As an example of `repeat`, see `Process.filter`. We define
     * a convenience function here for composing this `Process`
     * with a `Process` that filters the output type `O`.
     */
    def filter(f: O => Boolean): Process[I,O] = 
      this |> Process.filter(f)

    def zip[O2](p: Process[I,O2]): Process[I,(O,O2)] = 
      Process.zip(this, p)

    /* 
     * Exercise 7: Implement `zipWithIndex`. 
     */
    def zipWithIndex: Process[I,(O,Int)] = 
      this zip (count map (_ - 1))

    /* Add `p` to the `fallback` of this `Process`. */
    def orElse(p: Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Await(recv,fb) => Await(recv, fb ++ p)
      case _ => this
    }

    /* 
     * Convenience function for switching to the fallback branch
     * a `Process`.
     */
    def disconnectIn: Process[I,O] = this match {
      case Await(recv,fb) => fb 
      case Halt() => Halt()
      case Emit(h, t) => emitAll(h, t.disconnectIn)
    }
  }

  object Process {

    import fpinscala.iomonad.{Trampoline => T}

    case class Emit[I,O](
        head: Seq[O], 
        tail: Process[I,O] = Halt[I,O]()) 
      extends Process[I,O]

    case class Await[I,O](
        recv: I => Process[I,O], 
        fallback: Process[I,O] = Halt[I,O]()) 
      extends Process[I,O]

    case class Halt[I,O]() extends Process[I,O]

    def emit[I,O](head: O, 
                   tail: Process[I,O] = Halt[I,O]()): Process[I,O] = 
      emitAll(Stream(head), tail)

    def emitAll[I,O](head: Seq[O], 
                     tail: Process[I,O] = Halt[I,O]()): Process[I,O] = 
      tail match {
        case Emit(h2, tl) => Emit(head ++ h2, tl) 
        case _ => Emit(head, tail)
      }
   
    // Process forms a monad, and we provide monad syntax for it
    
    import fpinscala.iomonad.Monad

    def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] = 
      new Monad[({ type f[x] = Process[I,x]})#f] {
        def unit[O](o: => O): Process[I,O] = emit(o) 
        def flatMap[O,O2](p: Process[I,O])(f: O => Process[I,O2]): Process[I,O2] = 
          p flatMap f
      }

    // enable monadic syntax for `Process` type
    implicit def toMonadic[I,O](a: Process[I,O]) = monad[I].toMonadic(a)

    /* 
     * We can convert any function `f: I => O` to a `Process[I,O]`. We
     * simply `Await`, then `Emit` the value received, transformed by 
     * `f`. 
     */
    def lift[I,O](f: I => O): Process[I,O] = 
      Await((i: I) => emit(f(i), lift(f)))

    /*
     * As an example of `repeat`, here's a definition of `filter` that 
     * uses `repeat`. 
     */
    def filter[I](f: I => Boolean): Process[I,I] =
      Await[I,I](i => if (f(i)) emit(i) else Halt()) repeat 

    /* 
     * Here's a typical `Process` definition that requires tracking some 
     * piece of state (in this case, the running total):
     */
    def sum: Process[Double,Double] = {
      def go(acc: Double): Process[Double,Double] = 
        Await((d: Double) => emit(d+acc, go(d+acc))) 
      go(0.0)
    }

    /*
     * Exercise 2: Implement `take`, `drop`, `takeWhile`, and `dropWhile`.
     */
    def take[I](n: Int): Process[I,I] = 
      if (n <= 0) Halt()
      else Await(i => emit(i, take[I](n-1)))

    def drop[I](n: Int): Process[I,I] = 
      if (n <= 0) id
      else Await(i => drop[I](n-1))

    def takeWhile[I](f: I => Boolean): Process[I,I] = 
      Await(i => 
        if (f(i)) emit(i, takeWhile(f)) 
        else      Halt())

    def dropWhile[I](f: I => Boolean): Process[I,I] = 
      Await(i => 
        if (f(i)) dropWhile(f) 
        else      id)
    
    /* The identity `Process`, just repeatedly echos its input. */
    def id[I]: Process[I,I] = lift(identity)
    
    /*
     * Exercise 3: Implement `count`.
     * 
     * Here's one implementation, with three stages - we map all inputs
     * to 1.0, compute a running sum, then finally convert the output 
     * back to `Int`. The three stages will be interleaved - as soon
     * as the first element is examined, it will be converted to 1.0,
     * then added to the running total, and then this running total
     * will be converted back to `Int`, then the `Process` will examine
     * the next element, and so on.
     */
    def count[I]: Process[I,Int] = 
      lift((i: I) => 1.0) |> sum |> lift(_.toInt)

    /* For comparison, here is an explicit recursive implementation. */
    def count2[I]: Process[I,Int] = {
      def go(n: Int): Process[I,Int] = 
        Await((i: I) => emit(n+1, go(n+1)))
      go(0)
    }
    
    /* 
     * Exercise 4: Implement `mean`.
     * 
     * This is an explicit recursive definition. We'll factor out a 
     * generic combinator shortly.
     */
    def mean: Process[Double,Double] = {
      def go(sum: Double, count: Double): Process[Double,Double] = 
        Await((d: Double) => emit((sum+d) / (count+1), go(sum+d,count+1)))
      go(0.0, 0.0)
    }

    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
      Await((i: I) => f(i,z) match { 
        case (o,s2) => emit(o, loop(s2)(f))
      })

    /* Exercise 5: Implement `sum` and `count` in terms of `loop` */

    def sum2: Process[Double,Double] = 
      loop(0.0)((d:Double, acc) => (acc+d,acc+d))

    def count3[I]: Process[I,Int] = 
      loop(0)((_:I,n) => (n+1,n+1))

    /* 
     * Exercise 6: Can you think of a generic combinator that would 
     * allow for the definition of `mean` in terms of `sum` and 
     * `count`?
     *
     * Yes, it is `zip`, which feeds the same input to two processes. 
     * We use a helper function, `zipResidual`, defined in `These.scala`.
     */
    def zip[A,B,C](p1: Process[A,B], p2: Process[A,C]): Process[A,(B,C)] =
      (p1, p2) match {
        // if both are emitting, we zip together corresponding values,
        // taking care to handle case where the two processes are 
        // emitting different numbers of values
        case (Emit(bs, t1), Emit(cs, t2)) => 
          val (z, rema, remb) = These.zipResidual(bs, cs)
          Emit(z, zip(
            if (rema.isEmpty) t1 else Emit(rema, t1),
            if (remb.isEmpty) t2 else Emit(remb, t2) 
          ))
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        // if either side is Await-ing, we do the Await, then zip once
        // we have the result
        case (Await(recv1, fb1), _) =>
          Await(recv1 andThen (p1Next => zip(p1Next, p2)), zip (fb1, p2))
        case (_, Await(recv2, fb2)) =>
          Await(recv2 andThen (p2Next => zip(p1, p2Next)), zip (p1, fb2))
      }

    /* 
     * Using zip, we can then define `mean`. Again, this definition
     * operates in a single pass. 
     */
    val mean2 = (sum zip count) |> lift { case (s,n) => s / n }
    
    /* 
     * Exercise 7: Implement `zipWithIndex`. 
     * 
     * See definition on `Process` above.
     */
    
    /* 
     * Exercise 8: Implement `exists`
     * 
     * We choose to emit all intermediate values, and not halt.
     * See `existsResult` below for a trimmed version.
     */
    def exists[I](f: I => Boolean): Process[I,Boolean] =
      lift(f) |> any
    
    /* Emits whether a `true` input has ever been received. */
    def any: Process[Boolean,Boolean] = 
      loop(false)((b:Boolean,s) => (s || b, s || b))
    
    /* A trimmed `exists`, containing just the final result. */
    def existsResult[I](f: I => Boolean) = 
      exists(f) |> takeThrough(!_) |> dropWhile(!_) |> echo.orElse(emit(false))

    /* 
     * Like `takeWhile`, but includes the first element that tests 
     * false. 
     */
    def takeThrough[I](f: I => Boolean): Process[I,I] = 
      takeWhile(f) ++ echo

    /* Awaits then emits a single value, then halts. */
    def echo[I]: Process[I,I] = Await(i => emit(i)) 

    def skip[I,O]: Process[I,O] = Await(i => Halt())
    def ignore[I,O]: Process[I,O] = skip.repeat 

    def terminated[I]: Process[I,Option[I]] = 
      Await((i: I) => emit(Some(i), terminated[I]), emit(None))
  }
                            /*                       
  
  To implement our task of checking whether a file contains more than 
  40k, we need some way of binding a `Process` to talk to external  
  streams. Our first attempt is to define a separate type, `Source`, 
  which combines a `Process` with some effectful external source. We'll
  see later that this approach doesn't scale very well. 

                             */
  

  trait Source[O] {
    def |>[O2](p: Process[O,O2]): Source[O2]
    def filter(f: O => Boolean) = this |> Process.filter(f)
    def map[O2](f: O => O2) = this |> Process.lift(f)
    // can implement other functions on `Source` just using `|>`
    def take(n: Int) = this |> Process.take(n)
    def takeWhile(f: O => Boolean) = this |> Process.takeWhile(f)
    // etc

    def collect: IO[IndexedSeq[O]]
  }

  object Source {

    import Process._

    case class ResourceR[R,I,O]( // A resource from which we can read values
      acquire: IO[R],
      release: R => IO[Unit],
      step: R => IO[Option[I]],
      trans: Process[I,O]) extends Source[O] {
      def |>[O2](p: Process[O,O2]) = 
        ResourceR(acquire, release, step, trans |> p)

      /* 
       * Notice we are guaranteed to run the `release` action, whether 
       * we terminate normally or if an exception occurs during 
       * processing.
       */
      def collect: IO[IndexedSeq[O]] = {

        // Evaluate `a`, and run `cleanup` if this throws an exception
        def tryOr[A](a: => A)(cleanup: IO[Unit]) = 
          try a catch { case e: Exception => cleanup.run; throw e }

        @annotation.tailrec
        def go(acc: IndexedSeq[O],
               step: IO[Option[I]], 
               p: Process[I,O], 
               release: IO[Unit]): IndexedSeq[O] = 
          p match {
            case Halt() => release.run; acc 
            case Emit(h, t) => 
              // We `tryOr(acc ++ h)` since `h` may be a 
              // non-strict `Seq` like `Stream` which forces some 
              // computations that can fail.
              go(tryOr(acc ++ h)(release), step, t, release)
            case Await(recv, fb) => tryOr(step.run)(release) match {
              case None => go(acc, IO(None), fb, release) 
              case Some(i) => go(acc, step, recv(i), release)
            }
          }
        acquire map (res => 
          go(IndexedSeq(), step(res), trans, release(res)))
      }

      def run: IO[Unit] = (this |> ignore[O,Unit]).collect as ()
    }

    def lines(filename: String): Source[String] = 
      ResourceR(
        IO(io.Source.fromFile(filename)),
        (src: io.Source) => IO(src.close),
        (src: io.Source) => {
          lazy val iter = src.getLines  
          IO { if (iter.hasNext) Some(iter.next) else None }
        },
        Process.id[String])
  }

                            /*                       
  
  So far, our library cannot express programs that must incrementally 
  write to some _sink_, like a file. For instance, consider the
  following task:
  
    Transform `fahrenheit.txt`, a file containing temperatures in 
    degrees fahrenheit, to `celsius.txt`, a file containing the same 
    temperatures in degrees celsius.

  We can produce the source, but no way to write the output to another
  file!
                             */

  import fpinscala.iomonad.IO0.fahrenheitToCelsius 

  val tempsC: Source[Double] = 
    Source.lines("fahrenheit.txt").
           filter(!_.startsWith("#")).
           map(s => fahrenheitToCelsius(s.toDouble))

                            /*                       
  
  When we encounter limitations like this, we _can_ add more special
  cases to our `Source` type. Here, we define a notion of a `Sink`,
  and incorporate it into the `Source` type. 
  
                             */

  trait Sink[I] {
    def <|[I0](p: Process[I0,I]): Sink[I0]
    def filter(f: I => Boolean) = this <| Process.filter(f)
    // other combinators are similar, defined in terms of `<|`
  }

  object Sink {

    case class ResourceW[R,I,I2](
        acquire: IO[R], 
        release: R => IO[Unit],
        recv: R => (I2 => IO[Unit]),
        trans: Process[I,I2]) extends Sink[I] {

      def <|[I0](p: Process[I0,I]) = 
        ResourceW(acquire, release, recv, p |> trans)
    }

    import java.io.FileWriter
    
    /* An example `Sink`, for writing to a file. */
    def file(filename: String, append: Boolean = false): Sink[String] =
      ResourceW(
        IO(new FileWriter(filename, append)),
        (w: FileWriter) => IO(w.close),
        (w: FileWriter) => (s: String) => IO(w.write(s)),
        Process.id[String]
      )
  }

                            /*                       
  
  To integrate this into our `Source` API, let's imagine a
  new combinator, `observe`:

    def observe(snk: Sink[O]): Source[O]
  
  Implementing this combinator will likely require an additional 
  `Source` constructor and updates to our `collect` implementation.
  Assuming we can do this, the complete scenario then looks something
  like:

    val convert: IO[Unit] =
      Source.lines("fahrenheit.txt").
             filter(!_.startsWith("#")).
             map(s => fahrenheitToCelsius(s.toDouble)).
             map(d => d.toString + "\n"). ~ add line separators back in
             observe(Sink.file("celsius.txt")).
             run
   
   Ultimately, this approach of adding special cases to `Source`
   doesn't scale. See the chapter text for details. Next, we develop a 
   generalized `Process` type that lets us express sources, sinks,
   multiple inputs, and more complex usage scenarios than the ones
   we've considered so far.
                             */
}

object GeneralizedStreamTransducers {

                            /*                       
  
  Our generalized process type is parameterized on the protocol used for
  communicating with the driver. This works similarly to the `IO` type
  we defined in chapter 13. The `Await` constructor emits a request of
  type `F[A]`, and receives a response of type `A`:
    
    trait Process[F,A]
    case class Await[F[_],A,O](
      req: F[A], recv: A => Process[F,O],
      fallback: Process[F,O],
      finalizer: Process[F,O]) extends Process[F,O]

                             */
  
  trait Process[F[_],O] {
    import Process._ 
    
    /* 
     * Many of the same operations can be defined for this generalized
     * `Process` type, regardless of the choice of `F`.
     */

    def map[O2](f: O => O2): Process[F,O2] = this match {
      case Await(req,recv,fb,c) => 
        Await(req, recv andThen (_ map f), fb map f, c map f) 
      case Emit(h, t) => Emit(h map f, t map f)
      case Halt() => Halt()
    }

    def ++(p: => Process[F,O]): Process[F,O] = this match {
      case Halt() => p
      case Emit(h, t) => emitAll(h, t ++ p)
      case Await(req,recv,fb,c) => 
        Await(req, recv andThen (_ ++ p), fb ++ p, c)
    }

    def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] =
      this match {
        case Halt() => Halt()
        case Emit(o, t) => 
          if (o.isEmpty) t.flatMap(f)
          else f(o.head) ++ emitAll(o.tail, t).flatMap(f)
        case Await(req,recv,fb,c) => 
          Await(req, recv andThen (_ flatMap f), fb flatMap f, c flatMap f)
      }

    def repeat: Process[F,O] = {
      def go(p: Process[F,O]): Process[F,O] = p match {
        case Halt() => go(this)
        case Await(req,recv,fb,c) => Await(req, recv andThen go, fb, c)
        case Emit(h, t) => emitAll(h, go(t))
      }
      go(this)
    }

    /* 
     * This function is defined only if given a `Monad[F]` and a 
     * `Partial[F]`. Unlike the simple `collect` interpreter defined 
     * in the companion object below, this is not tail recursive and
     * responsibility for stack safety is placed on the `Monad`
     * instance.
     */
    def collect(implicit F: Monad[F], P: Partial[F]): F[IndexedSeq[O]] = {
      def go(cur: Process[F,O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = 
        cur match {
          case Emit(h,t) => go(t, acc ++ h) 
          case Halt() => F.unit(acc)
          case Await(req,recv,fb,c) => 
             F.flatMap (P.attempt(req)) {
               case Left(End) => go(fb, acc)
               case Left(err) => 
                 go(c ++ await[F,Nothing,O](P.fail(err))(), acc)
               case Right(o) => go(recv(o), acc)
             }
        }
      go(this, IndexedSeq())
    }
    
    /* 
     * We define `Process1` as a type alias - see the companion object
     * for `Process` below. Using that, we can then define `|>` once
     * more. The definition is extremely similar to our previous 
     * definition. 
     * 
     * The one subtlety is we make sure that if `p2` halts, we
     * `kill` this process, giving it a chance to run any cleanup
     * actions (like closing file handles, etc). 
     */
    def |>[O2](p2: Process1[O,O2]): Process[F,O2] = {
      // if this is emitting values and p2 is consuming values,
      // we feed p2 in a loop to avoid using stack space
      @annotation.tailrec
      def feed(emit: Seq[O], tail: Process[F,O], 
               recv: O => Process1[O,O2], 
               fb: Process1[O,O2],
               cleanup: Process1[O,O2]): Process[F,O2] = 
        if (emit isEmpty) tail |> await1(recv, fb) 
        else recv(emit.head) match {
          case Await(_, recv2, fb2, c2) => 
            feed(emit.tail, tail, recv2, fb2, c2)
          case p => emitAll(emit.tail, tail) |> p
        }
      p2 match {
        case Halt() => this.kill ++ Halt()
        case Emit(h, t) => emitAll(h, this |> t)
        case Await(req,recv,fb,c) => this match {
          case Emit(h,t) => feed(h, t, recv, fb, c)
          case Halt() => Halt() |> fb
          case Await(req0,recv0,fb0,c0) => 
            await(req0)(i => recv0(i) |> p2, 
                        fb0 |> fb,
                        c0 |> c)
        }
      }
    }

    @annotation.tailrec
    final def kill[O2]: Process[F,O2] = this match {
      case Await(req,recv,fb,c) => c.drain 
      case Halt() => Halt()
      case Emit(h, t) => t.kill
    }

    final def drain[O2]: Process[F,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => t.drain
      case Await(req,recv,fb,c) => Await(
        req, recv andThen (_.drain), 
        fb.drain, c.drain) 
    }

    def filter(f: O => Boolean): Process[F,O] = 
      this |> Process.filter(f)

    def take(n: Int): Process[F,O] = 
      this |> Process.take(n)

    def once: Process[F,O] = take(1)

    /* 
     * Use a `Tee` to interleave or combine the outputs of `this` and
     * `p2`. This can be used for zipping, interleaving, and so forth.
     * Nothing requires that the `Tee` read elements from each 
     * `Process` in lockstep. It could read fifty elements from one 
     * side, then two elements from the other, then combine or
     * interleave these values in some way, etc.
     * 
     * This definition uses two helper functions, `feedL` and `feedR`,
     * which feed the `Tee` in a tail-recursive loop as long as
     * it is awaiting input.
     */ 
    def tee[O2,O3](p2: Process[F,O2])(t: Tee[O,O2,O3]): Process[F,O3] = {
      @annotation.tailrec
      def feedL(emit: Seq[O], tail: Process[F,O], 
                other: Process[F,O2],
                recv: O => Tee[O,O2,O3], 
                fb: Tee[O,O2,O3],
                c: Tee[O,O2,O3]): Process[F,O3] = 
        if (emit isEmpty) (tail tee other)(await(L[O,O2])(recv, fb, c))
        else recv(emit.head) match {
          case t2@Await(e, recv2, fb2, c2) => e.get match {
            case Left(_) => feedL(emit.tail, tail, other, recv2, fb2, c2)
            case _ => (Emit(emit.tail, tail) tee other)(t2)
          }
          case p => (Emit(emit.tail, tail) tee other)(p)
        }
      @annotation.tailrec
      def feedR(emit: Seq[O2], tail: Process[F,O2], 
                other: Process[F,O],
                recv: O2 => Tee[O,O2,O3], 
                fb: Tee[O,O2,O3],
                c: Tee[O,O2,O3]): Process[F,O3] = 
        if (emit isEmpty) (other tee tail)(await(R[O,O2])(recv, fb, c))
        else recv(emit.head) match {
          case t2@Await(e, recv2, fb2, c2) => e.get match {
            case Right(_) => feedR(emit.tail, tail, other, recv2, fb2, c2)
            case _ => (other tee Emit(emit.tail, tail))(t2)
          }
          case p => (other tee Emit(emit.tail, tail))(p)
        }
      t match {
        case Halt() => this.kill ++ p2.kill ++ Halt() 
        case Emit(h,t) => Emit(h, (this tee p2)(t))
        case Await(side, recv, fb, c) => side.get match {
          case Left(isO) => this match {
            case Halt() => p2.kill ++ Halt()
            case Emit(o,ot) => feedL(o, ot, p2, isO.to andThen recv, fb, c) 
            case Await(reqL, recvL, fbL, cL) => 
              Await(reqL, recvL andThen (this2 => (this2 tee p2)(t)), 
                    (fbL tee p2)(t), (cL tee p2)(t))
          }
          case Right(isO2) => p2 match {
            case Halt() => this.kill ++ Halt()
            case Emit(o,ot) => feedR(o, ot, this, isO2.to andThen recv, fb, c)
            case Await(reqR, recvR, fbR, cR) => 
              Await(reqR, recvR andThen (p3 => (this tee p3)(t)), 
                    (this tee fbR)(t), (this tee cR)(t))
          }
        }
      }
    }

    def zipWith[O2,O3](p2: Process[F,O2])(f: (O,O2) => O3): Process[F,O3] = 
      (this tee p2)(Process.zipWith(f))

    def zip[O2](p2: Process[F,O2]): Process[F,(O,O2)] = 
      zipWith(p2)((_,_))

    def to[O2](sink: Sink[F,O]): Process[F,Unit] = 
      eval { (this zipWith sink)((o,f) => f(o)) }

    def through[O2](p2: Process[F, O => F[O2]]): Process[F,O2] = 
      eval { (this zipWith p2)((o,f) => f(o)) }

    def disconnectIn: Process[F,O] = this match {
      case Await(req,recv,fb,c) => fb 
      case Halt() => Halt()
      case Emit(h, t) => Emit(h, t.disconnectIn)
    }

    def disconnect[O2]: Process[F,O2] = 
      this.disconnectIn.drain

    def wye[O2,O3](p2: Process[F,O2])(y: Wye[O,O2,O3])(
                   implicit F: Nondeterminism[F]): Process[F,O3] =
      ???

    def channel[O2,O3](p2: Process[F,O => F[O2]])(
                    y: Wye[O, O => O2, O3]): Process[F,O3] = ???
  }

  object Process {
    case class Await[F[_],A,O](
      req: F[A], recv: A => Process[F,O],
      fallback: Process[F,O],
      cleanup: Process[F,O]) extends Process[F,O]
  
    case class Emit[F[_],O](
      head: Seq[O], 
      tail: Process[F,O]) extends Process[F,O]

    case class Halt[F[_],O]() extends Process[F,O]

    def emitAll[F[_],O](
        head: Seq[O], 
        tail: Process[F,O] = Halt[F,O]()): Process[F,O] = 
      tail match {
        case Emit(h2,t) => Emit(head ++ h2, t)
        case _ => Emit(head, tail)
      }
    def emit[F[_],O](
        head: O, 
        tail: Process[F,O] = Halt[F,O]()): Process[F,O] = 
      emitAll(Stream(head), tail)

    def await[F[_],A,O](req: F[A])(
        recv: A => Process[F,O] = (a: A) => Halt[F,O](), 
        fallback: Process[F,O] = Halt[F,O](),
        cleanup: Process[F,O] = Halt[F,O]()): Process[F,O] = 
      Await(req, recv, fallback, cleanup)

    /* Our generalized `Process` type can represent sources! */

    type IO[A] = fpinscala.iomonad.IO[Task,A]
    
    /* Special exception indicating normal termination */
    case object End extends Exception
   
    /* 
     * Here is a simple tail recursive function to collect all the
     * output of a `Process[IO,O]`. Notice we are using the fact
     * that `IO` can be `run` to produce either a result or an
     * exception.
     */
    def collect[O](src: Process[IO,O]): IndexedSeq[O] = {
      @annotation.tailrec
      def go(cur: Process[IO,O], acc: IndexedSeq[O]): IndexedSeq[O] = 
        cur match {
          case Emit(h,t) => go(t, acc ++ h) 
          case Halt() => acc
          case Await(req,recv,fb,err) =>
            val next = 
              try recv(req.run)
              catch { 
                case End => fb // Normal termination
                case e: Exception => err ++ failIO(e) // Helper function, defined below
              }
            go(next, acc)
        }
      go(src, IndexedSeq()) 
    }

    def failIO[O](e: Throwable): Process[IO,O] = 
      await[IO,O,O](IO(throw e))()

    /* 
     * We can write a version of collect that works for any `Monad`. 
     * See the definition in the body of `Process`. 
     */

    /* 
     * Generic combinator for producing a `Process[IO,O]` from some
     * effectful `O` source. The source is tied to some resource,
     * `R` (like a file handle) that we want to ensure is released.
     * See `lines` below for an example use. 
     */
    def resource[R,O](acquire: IO[R])(
                      release: R => IO[Unit])(
                      step: R => IO[O]): Process[IO,O] = {
      def go(step: IO[O], onExit: IO[Unit]): Process[IO,O] =
        await[IO,O,O](step) ( 
          o => emit(o, go(step, onExit)) // Emit the value and repeat 
        , await[IO,Unit,O](onExit)()  // Release resource when exhausted
        , await[IO,Unit,O](onExit)()) // or in event of error
      await(acquire) ( r => go(step(r), release(r)), Halt(), Halt() )
    }

    /* 
     * Create a `Process[IO,O]` from the lines of a file, using
     * the `resource` combinator above to ensure the file is closed
     * when processing the stream of lines is finished. 
     */
    def lines(filename: String): Process[IO,String] = 
      resource(IO(io.Source.fromFile(filename)))(
               src => IO(src.close)) { src => 
        lazy val lines = src.getLines // A stateful iterator 
        IO { if (lines.hasNext) lines.next else throw End }
      }
    
    /* 
     * We now have nice, resource safe effectful sources, but we don't 
     * have any way to transform them or filter them. Luckily we can 
     * still represent the single-input `Process` type we introduced
     * earlier, which we'll now call `Process1`.  
     */

    case class Is[I]() {
      sealed trait f[X] { def is: Eq[X,I] } // see definition in `Eq.scala`
      val Get = new f[I] { def is = Eq.refl }
    }
    def Get[I] = Is[I]().Get

    type Process1[I,O] = Process[Is[I]#f, O]
    
    def halt1[I,O]: Process1[I,O] = Halt[Is[I]#f, O]()

    def await1[I,O](
      recv: I => Process1[I,O],
      fallback: Process1[I,O] = halt1[I,O]): Process1[I, O]  = 
      Await(Get[I], recv, fallback, halt1)

    def emit1[I,O](h: O, tl: Process1[I,O] = halt1[I,O]): Process1[I,O] = 
      emit(h, tl)
    
    def emitAll1[I,O](h: Seq[O], tl: Process1[I,O] = halt1[I,O]): Process1[I,O] = 
      emitAll(h, tl)

    def lift[I,O](f: I => O): Process1[I,O] = 
      await1[I,O]((i:I) => emit(f(i))) repeat
    
    def filter[I](f: I => Boolean): Process1[I,I] =
      await1[I,I](i => if (f(i)) emit(i) else halt1) repeat

    // we can define take, takeWhile, and so on as before

    def take[I](n: Int): Process1[I,I] = 
      if (n <= 0) halt1
      else await1[I,I](i => emit(i, take(n-1)))

                            /*                       
  
    We sometimes need to construct a `Process` that will pull values
    from multiple input sources. For instance, suppose we want to 
    'zip' together two files, `f1.txt` and `f2.txt`, combining
    corresponding lines in some way. Using the same trick we used for
    `Process1`, we can create a two-input `Process` which can request
    values from either the 'left' stream or the 'right' stream. We'll
    call this a `Tee`, after the letter 'T', which looks like a 
    little diagram of two inputs being combined into one output. 

                             */

    case class T[I,I2]() {
      sealed trait f[X] { def get: Either[Eq[X,I], Eq[X,I2]] }
      val L = new f[I] { def get = Left(Eq.refl) }
      val R = new f[I2] { def get = Right(Eq.refl) }
    }
    def L[I,I2] = T[I,I2]().L
    def R[I,I2] = T[I,I2]().R

    type Tee[I,I2,O] = Process[T[I,I2]#f, O]
    
    /* Again some helper functions to improve type inference. */

    def awaitL[I,I2,O](
        recv: I => Tee[I,I2,O], 
        fallback: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
      await[T[I,I2]#f,I,O](L)(recv, fallback)

    def awaitR[I,I2,O](
        recv: I2 => Tee[I,I2,O], 
        fallback: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
      await[T[I,I2]#f,I2,O](R)(recv, fallback)

    def haltT[I,I2,O]: Tee[I,I2,O] = 
      Halt[T[I,I2]#f,O]()

    def emitT[I,I2,O](h: O, tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
      emit(h, tl)
    
    def emitAllT[I,I2,O](h: Seq[O], tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] = 
      emitAll(h, tl)

    def zipWith[I,I2,O](f: (I,I2) => O): Tee[I,I2,O] = 
      awaitL[I,I2,O](i  => 
      awaitR        (i2 => emitT(f(i,i2)))) repeat

    def zip[I,I2]: Tee[I,I2,(I,I2)] = zipWith((_,_))

    /* 
     * Like `zip` on lists, the above version halts as soon as either
     * input is exhausted. Here is a version that pads the shorter
     * stream with values. 
     */
     
    def zipWithAll[I,I2,O](padI: I, padI2: I2)(
                           f: (I,I2) => O): Tee[I,I2,O] = {
      val fbR = passR[I,I2] map (f(padI, _    ))                     
      val fbL = passL[I,I2] map (f(_   , padI2))
      awaitLOr(fbR)(i => 
      awaitROr(fbL)(i2 => emitT(f(i,i2)))) repeat
    }

    def zipAll[I,I2](padI: I, padI2: I2): Tee[I,I2,(I,I2)] = 
      zipWithAll(padI, padI2)((_,_))
    
    def awaitLOr[I,I2,O](fallback: Tee[I,I2,O])(
                         recvL: I => Tee[I,I2,O]): Tee[I,I2,O] =
      awaitL(recvL, fallback)

    def awaitROr[I,I2,O](fallback: Tee[I,I2,O])(
                         recvR: I2 => Tee[I,I2,O]): Tee[I,I2,O] =
      awaitR(recvR, fallback)

    /* Ignores all input from left. */
    def passR[I,I2]: Tee[I,I2,I2] = awaitR(emitT(_, passR))
    
    /* Ignores input from the right. */
    def passL[I,I2]: Tee[I,I2,I] = awaitL(emitT(_, passL))
    
    /* Alternate pulling values from the left and the right inputs. */
    def interleaveT[I]: Tee[I,I,I] = 
      awaitL[I,I,I](i =>
      awaitR       (i2 => emitAllT(Seq(i,i2)))) repeat

                            /*                       
  
    Our `Process` type can also represent effectful sinks (like a file).
    A `Sink` is simply a source of effectful functions! See the
    definition of `to` in `Process` for an example of how to feed a 
    `Process` to a `Sink`.

                             */

    type Sink[F[_],O] = Process[F, O => F[Unit]]

    /* A `Sink` which writes input strings to the given file. */
    def fileW(file: String, append: Boolean = false): Sink[IO,String] = 
      resource(IO { new java.io.FileWriter(file, append) })(
               w => IO(w.close)) {
        w => IO { (s: String) => IO(w.write(s)) } 
      }

    /* Exercise 9: Implement `eval`. */

    def eval[F[_],O](p: Process[F, F[O]]): Process[F,O] = 
      p match {
        case Halt() => Halt()
        case Emit(h, t) => 
          if (h.isEmpty) eval(t)
          else await[F,O,O](h.head)(o => emit(o, eval(emitAll(h.tail, t))))
        case Await(req,recv,fb,c) => 
          await(req)(recv andThen eval, eval(fb), eval(c)) 
      }
    
    /* Infix syntax for `eval`. */
    implicit class EvalProcess[F[_],O](self: Process[F,F[O]]) {
      def eval = Process.eval(self) 
    }

    /* 
     * An example use of the combinators we have so far: incrementally 
     * convert the lines of a file from fahrenheit to celsius.
     */

    import fpinscala.iomonad.IO0.fahrenheitToCelsius 

    val converter: Process[IO,Unit] = 
      lines("fahrenheit.txt").
      filter(!_.startsWith("#")).
      map(line => fahrenheitToCelsius(line.toDouble).toString).
      to(fileW("celsius.txt")).
      drain
    
                            /*                       
  
    More generally, we can feed a `Process` through an effectful 
    channel which returns a value other than `Unit`.

                             */
    
    type Channel[F[_],I,O] = Process[F, I => F[O]]

    /* 
     * Here is an example, a JDBC query runner which returns the
     * stream of rows from the result set of the query. We have
     * the channel take a `Connection => PreparedStatement` as 
     * input, so code that uses this channel does not need to be
     * responsible for knowing how to obtain a `Connection`.
     */
    import java.sql.{Connection, PreparedStatement, ResultSet}

    def query(conn: IO[Connection]): 
        Channel[IO, Connection => PreparedStatement, 
                    Process[IO,Map[String,Any]]] = 
      resource(conn)(c => IO(c.close)) { conn => IO { 
        (q: Connection => PreparedStatement) => {
          IO { resource ( IO { 
            val rs = q(conn).executeQuery
            val ncols = rs.getMetaData.getColumnCount
            val colnames = (1 to ncols).map(rs.getMetaData.getColumnName)
            (rs, colnames)
          }) ( p => IO { p._1.close } ) { // close the ResultSet 
            case (rs, cols) => IO { 
              if (!rs.next) throw End 
              else cols.map(c => (c, rs.getObject(c): Any)).toMap 
            }
          }}
        }
      }}

    /* 
     * We can allocate resources dynamically when defining a `Process`.
     * As an example, this program reads a list of filenames to process
     * _from another file_, opening each file, processing it and closing
     * it promptly.
     */

    val convertAll: Process[IO,Unit] = (for {
      out <- fileW("celsius.txt").once
      file <- lines("fahrenheits.txt") 
      _ <- lines(file).
           map(line => fahrenheitToCelsius(line.toDouble)).
           map(celsius => out(celsius.toString)).
           eval // see definition of infix syntax above
    } yield ()) drain 

    /* 
     * Just by switching the order of the `flatMap` calls, we can output
     * to multiple files. 
     */
    val convertMultisink: Process[IO,Unit] = (for {
      file <- lines("fahrenheits.txt") 
      _ <- lines(file).
           map(line => fahrenheitToCelsius(line.toDouble)).
           map(_ toString).
           to(fileW(file + ".celsius"))
    } yield ()) drain
    
    /* 
     * We can attach filters or other transformations at any point in the
     * program, for example:
     */
    val convertMultisink2: Process[IO,Unit] = (for {
      file <- lines("fahrenheits.txt") 
      _ <- lines(file).
           filter(!_.startsWith("#")).
           map(line => fahrenheitToCelsius(line.toDouble)).
           filter(_ > 0). // ignore below zero temperatures
           map(_ toString).
           to(fileW(file + ".celsius"))
    } yield ()) drain


    import These._

    case class Y[I,I2]() {
      sealed trait f[X] { 
        def get: (X =:= I) Either 
                 (X =:= I2) Either 
                 (X =:= These[I,I2])
      }
      val A = new f[I] { def get = Left(Left(implicitly)) }
      val B = new f[I2] { def get = Left(Right(implicitly)) }
      val AB = new f[These[I,I2]] { def get = Right(implicitly) }
    }
    def A[I,I2] = Y[I,I2]().A
    def B[I,I2] = Y[I,I2]().B
    def AB[I,I2] = Y[I,I2]().AB 
    
    type Wye[I,I2,O] = Process[Y[I,I2]#f, O]
    
    def haltY[I,I2,O]: Wye[I,I2,O] = 
      Halt[Y[I,I2]#f,O]()
    
    def emitY[I,I2,O](h: O, tl: Wye[I,I2,O] = haltY[I,I2,O]): Wye[I,I2,O] = 
      emit(h, tl)

    def awaitA[I,I2,O](
        recv: I => Wye[I,I2,O], 
        fallback: Wye[I,I2,O] = haltY[I,I2,O]): Wye[I,I2,O] = 
      await[Y[I,I2]#f,I,O](A)(recv, fallback)

    def awaitB[I,I2,O](
        recv: I2 => Wye[I,I2,O], 
        fallback: Wye[I,I2,O] = haltY[I,I2,O]): Wye[I,I2,O] = 
      await[Y[I,I2]#f,I2,O](B)(recv, fallback)

    def awaitAB[I,I2,O](
        recv: These[I,I2] => Wye[I,I2,O], 
        fallback: Wye[I,I2,O] = haltY[I,I2,O]): Wye[I,I2,O] = 
      await[Y[I,I2]#f,These[I,I2],O](AB)(recv, fallback)

    def zipWithY[O,O2,O3](f: (O,O2) => O3): Wye[O,O2,O3] = 
      awaitAB[O,O2,O3](o => 
        o match {
          case That(o2) => awaitA(o => emitY(f(o,o2))) 
          case This(o) => awaitB(o2 => emitY(f(o,o2)))
          case Both(o,o2) => emitY(f(o,o2))
        }).repeat

    trait Nondeterminism[F[_]] {
      def choose[A,B](a: F[A], b: F[B]): 
        F[(A, F[B]) Either (F[A], B) ]
      def choose[A](a: Seq[F[A]]): F[(A, Seq[F[A]])] 
    }
    
  }
}

object ProcessTest extends App {
  import GeneralizedStreamTransducers._
  import Process.{IO, lines}

  println { Process.collect(Process.converter) }
  println { Process.collect(Process.convertAll) }
}
