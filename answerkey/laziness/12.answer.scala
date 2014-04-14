/*
Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
*/
val fibsViaUnfold = 
  unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

def fromViaUnfold(n: Int) = 
  unfold(n)(n => Some((n,n+1)))

def constantViaUnfold[A](a: A) = 
  unfold(a)(_ => Some((a,a)))

// could also of course be implemented as constant(1)
val onesViaUnfold = unfold(1)(_ => Some((1,1)))