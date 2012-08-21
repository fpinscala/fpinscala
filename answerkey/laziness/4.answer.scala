/* 
Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found. 
*/
def forAll(f: A => Boolean): Boolean =
  foldRight(true)((a,b) => f(a) && b)