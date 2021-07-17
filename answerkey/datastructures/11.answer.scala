def sumViaFoldLeft(l: List[Int]) = foldLeft(l, 0, _ + _)
def productViaFoldLeft(l: List[Double]) = foldLeft(l, 1.0, _ * _)

def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc,h) => acc + 1)
