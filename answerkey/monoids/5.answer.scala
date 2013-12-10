def wordsMonoid: Monoid[String] = new Monoid[String]{
	def op(s1: String, s2: String): String = 
			if(s1 == zero) s2
			else if(s2 == zero) s1
			else (s1.trim + " " + s2.trim).trim
	val zero: String = " "
}
