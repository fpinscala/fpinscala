/* 
This could also be implemented directly using `foldRight`.
*/
def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
  concat(map(l)(f))