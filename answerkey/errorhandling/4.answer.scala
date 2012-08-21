def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
  map2(mkMatcher(pat1), mkMatcher(pat2))((f,g) => f(s) && g(s))

def mkMatcher(pat: String): Option[String => Boolean] = 
  pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.