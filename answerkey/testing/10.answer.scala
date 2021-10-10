extension [A](self: Gen[A]) def unsized: SGen[A] = 
  _ => self
