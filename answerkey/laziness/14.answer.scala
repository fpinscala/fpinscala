/* 
`s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
*/
def startsWith[A](s: Stream[A]): Boolean = 
  zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (h,h2) => h == h2
  }