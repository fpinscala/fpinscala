/* 
Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use this function to implement just about any recursive function that would otherwise be defined by pattern matching.
*/
def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
  case Leaf(a) => f(a)
  case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
}

def sizeViaFold[A](t: Tree[A]): Int = 
  fold(t)(a => 1)(1 + _ + _)

def maximumViaFold(t: Tree[Int]): Int = 
  fold(t)(a => a)(_ max _)

def depthViaFold[A](t: Tree[A]): Int = 
  fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

/*
Notice the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this: 

type mismatch;
  found   : fpinscala.datastructures.Branch[B]
  required: fpinscala.datastructures.Leaf[B]
     fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                    ^  

This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument to `fold` will return `Leaf[B]`, which it does not (it returns `Branch[B]`). Really, we would prefer if Scala would infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it is somewhat common to define helper functions that simply call the corresponding data constructors but give the less specific result type:  
  
  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
*/
def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = 
  fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))