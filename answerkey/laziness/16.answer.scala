/*
This implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
*/
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2


// Implementation via Stream.unfold, which performs more values evaluations.
  def scanRightViaUnfold[R](e: => R)(c: (=> E, => R) => R): Stream[R] = Stream.unfold(this) {
    case Empty => None
    case s => Some((s.foldRight(e)(c), s.tail))
  } append Stream(e)
