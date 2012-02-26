package fpinscala.exercises

trait Stream[A] {
  def uncons: Option[(A, Stream[A])]
  def toList: List[A] = 
    sys.error("todo")
}
