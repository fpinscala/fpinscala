def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
  map2(mkMatcher(pat1), mkMatcher(pat2))((f,g) => f(s) && g(s))