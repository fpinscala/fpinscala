// Here's one way to approach implementing this:
// from the return type, `B => C`, we can immediately
// begin by writing `(b: B) => ???`. We then look
// at what values we have in scope - we have an 
// `a: A`, a `b: B` (from the function parameter), 
// and an `f: (A,B) => C`. Our goal is to produce a
// `C`. There isn't anything we can do other than 
// pass `f` the `A` and the `B` that we have!
def partial1[A,B,C](a: A, f: (A,B) => C): B => C = 
  (b: B) => f(a, b)