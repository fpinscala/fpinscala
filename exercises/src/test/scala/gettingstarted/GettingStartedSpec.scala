import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.gettingstarted._

class FibSpec extends Specification{
	"my fib" should	{
		"return 0 for index 0" in {MyModule.fib(0) mustEqual 0}
	}
}