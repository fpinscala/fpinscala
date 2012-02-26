package fpinscala.answers

trait Stream[A] {
  def uncons: Option[(A, Stream[A])]
  def toList: List[A] = 
      // note - not tail recursive
      uncons.map { case (h,t) => h :: t.toList } getOrElse List()
}
