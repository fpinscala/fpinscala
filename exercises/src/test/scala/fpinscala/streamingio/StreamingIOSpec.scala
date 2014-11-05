package fpinscala.streamingio

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import SimpleStreamTransducers.{Process => SSTProcess}
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StreamingIOSpec extends FlatSpec with PropertyChecks {

  private def between0AndN(n: Int) = Gen.chooseNum(0, n) label "n"

  behavior of "15.1.1 Process.take"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll (between0AndN(l.size)) { n: Int =>
        val result = SSTProcess.take(n)(l.toStream)
        assert(result.toList == l.take(n))
      }
    }
  }

  behavior of "15.1.2 Process.drop"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll (between0AndN(l.size)) { n: Int =>
        val result = SSTProcess.drop(n)(l.toStream)
        assert(result.toList == l.drop(n))
      }
    }
  }

  private def even(i: Int) = (i % 2) == 0

  behavior of "15.1.3 Process.takeWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.takeWhile(even)(l.toStream)
      assert(result.toList == l.takeWhile(even))
    }
  }

  behavior of "15.1.4 Process.dropWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.dropWhile(even)(l.toStream)
      assert(result.toList == l.dropWhile(even))
    }
  }

  behavior of "15.2 Process.count"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.count(l.toStream)
      assert(result.toList == l.zipWithIndex.map(_._2 + 1))
    }
  }
}
