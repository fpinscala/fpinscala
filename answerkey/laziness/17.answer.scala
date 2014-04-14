/*
The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.

The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
*/
def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] = 
  foldRight((z, Stream(z)))((a,p) => {
    val b2 = f(a,p._1)
    (b2, cons(b2,p._2))
  })._2