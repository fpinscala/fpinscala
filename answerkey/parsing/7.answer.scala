/* We'll just have this parser return the number of `"a"` characters read. Notice that we can declare a normal value inside a for-comprehension. */
for { 
  digit <- "[0-9]+".r
  val n = digit.toInt
  _ <- listOfN(n, char('a'))
} yield n