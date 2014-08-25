def headOption: Option[A] =
  foldRight(None: Option[A])((h,_) => Some(h))