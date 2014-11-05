package fpinscala.localeffects

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class LocalEffectsSpec extends FlatSpec with PropertyChecks {

  behavior of "14.1 STArray.fill"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      val intMap = ints.zipWithIndex.map(_.swap).toMap
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

  private val ints = {
    val intGen = Gen.choose(-10, 10)
    // nonempty List w/o duplicates
    Gen.choose(1, 10).flatMap(Gen.listOfN(_, intGen)).map(_.toSet).map(_.toList)
  } label "ints"

  behavior of "14.2.1 STArray.partition"
  it should "work" in {
    forAll(ints) { ints: List[Int] =>
      val l = 0
      val r = ints.size - 1
      val pivot = l + (r - l) / 2
      val pivotValue = ints(pivot)
      val partitions = ints.partition(_ <= pivotValue)
      val runnableST = new RunnableST[(List[Int], Int)] {
        override def apply[S] = for {
          array <- STArray.fromList[S, Int](ints)
          p <- Immutable.partition(array, l, r, pivot)
          list <- array.freeze
        } yield (list, p)
      }
      val result = ST.runST(runnableST)
      val resultPartition = result._1.splitAt(result._2 + 1)._1
      assert(resultPartition.toSet == partitions._1.toSet)
    }
  }

  behavior of "14.2.2 STArray.qs"
  it should "work" in {
    forAll(ints) { ints: List[Int] =>
      val l = 0
      val r = ints.size - 1
      val runnableST = new RunnableST[List[Int]] {
        override def apply[S] = for {
          array <- STArray.fromList[S, Int](ints)
          _ <- Immutable.qs(array, l, r)
          list <- array.freeze
        } yield list
      }
      val result = ST.runST(runnableST)
      assert(result == ints.sorted)
    }
  }
}
