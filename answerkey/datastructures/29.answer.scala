/* 
Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
accumulates some value using these handlers. As with `foldRight`, `t.fold(Leaf(_))(Branch(_,_)) == t`, and we can use
this function to implement just about any recursive function that would otherwise be defined by pattern matching.
*/
def fold[B](f: A => B, g: (B,B) => B): B = this match
  case Leaf(a) => f(a)
  case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

def sizeViaFold: Int = 
  fold(a => 1, 1 + _ + _)

def depthViaFold: Int = 
  fold(a => 0, (d1,d2) => 1 + (d1 max d2))

def mapViaFold[B](f: A => B): Tree[B] = 
  fold(a => Leaf(f(a)), Branch(_,_))
