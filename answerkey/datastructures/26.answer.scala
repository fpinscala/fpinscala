/*
We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.

Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise. 
*/
def maximum(t: Tree[Int]): Int = t match {
  case Leaf(n) => n
  case Branch(l,r) => maximum(l) max maximum(r)
}