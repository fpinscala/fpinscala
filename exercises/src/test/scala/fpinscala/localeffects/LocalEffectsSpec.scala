package fpinscala.localeffects

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class LocalEffectsSpec extends FlatSpec with PropertyChecks {

  behavior of "14.1 STArray.fill"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      val intMap = ints.zipWithIndex.map {case (a,i) => (i,a)}.toMap
      val runnableST  = new RunnableST[List[Int]] {
        override def apply[S] = for {
          array <- STArray[S, Int](ints.size, 0)
          _ <- array.fill(intMap)
          l <- array.freeze
        } yield l
      }
      val result = ST.runST(runnableST)
      assert(result == ints)
    }
  }
}
