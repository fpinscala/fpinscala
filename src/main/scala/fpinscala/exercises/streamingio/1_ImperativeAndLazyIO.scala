package fpinscala.exercises.streamingio

import fpinscala.answers.iomonad.IO

object ImperativeAndLazyIO:

                            /*

  We are going to consider various approaches to the simple task of
  checking whether a file contains more than 40,000 lines.

  Our first implementation is an imperative implementation, embedded
  into `IO`.
                             */

  import java.io.*

  def linesGt40k(filename: String): IO[Boolean] = IO {
    // There are a number of convenience functions in scala.io.Source
    // for reading from external sources such as files.
    val src = io.Source.fromFile(filename)
    try
      var count = 0
      // Obtain a stateful iterator from the Source
      val lines: Iterator[String] = src.getLines()
      while (count <= 40000 && lines.hasNext)
        lines.next() // has side effect of advancing to next element
        count += 1
      count > 40000
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

  Now imagine if we had a `LazyList[String]` for the lines of the file
  and we could assemble functionality using all the `LazyList` functions
  we know and love.
                             */

  object Examples:
    val lines: LazyList[String] = sys.error("defined elsewhere")
    val ex1 = lines.zipWithIndex.exists(_(1) + 1 >= 40000)
    val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_(1) + 1 >= 40000)
    val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)

                            /*

  Could we actually write the above? Not quite. We could 'cheat' and
  return an `IO[LazyList[String]]` representing the lines of a file:

                             */

  def lines(filename: String): IO[LazyList[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines().to(LazyList) ++ { src.close; LazyList.empty }
  }
                            /*

  This is called _lazy I/O_, and it's problematic for a number of
  reasons, discussed in the book text. However, it would be nice to
  recover the same high-level, compositional style we are used to
  from our use of `List` and `LazyList`.

                             */
end ImperativeAndLazyIO

