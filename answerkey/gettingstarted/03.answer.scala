// Polymorphic functions are often so constrained by their type
// that they only have one implementation! Here's an example:

def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
  (b: B) => f(a, b)