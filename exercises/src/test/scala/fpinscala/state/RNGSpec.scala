package fpinscala.state

import org.specs2._
import org.scalacheck._

class RNGSpec extends Specification with ScalaCheck { def is =

  "nonNegativeInt" ! check { (seed: Int) => RNG.nonNegativeInt(RNG.Simple(seed))._1 >= 0 }

}
