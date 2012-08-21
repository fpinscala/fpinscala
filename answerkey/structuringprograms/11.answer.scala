object Lift3ReusingLift {
  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E =
    a => lift[A,C,D,E](f(g(a), _, _))(h, i)(a)
}