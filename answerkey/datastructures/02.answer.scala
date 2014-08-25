/*
Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently returning a value just means this bug will be discovered later, further from the place where it was introduced. 

It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the right hand side of a pattern. This makes it clear the value isn't relevant.  
*/
def tail[A](l: List[A]): List[A] = 
  l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,t) => t
  }