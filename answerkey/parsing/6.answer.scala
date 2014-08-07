/* We'll just have this parser return the number of `"a"` characters read. Note that we can declare a normal value inside a for-comprehension. */
for {
  digit <- "[0-9]+".r
  val n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
  _ <- listOfN(n, char('a'))
} yield n

