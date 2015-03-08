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

  behavior of "14.3 STMap"
  it should "work with STMap.empty" in {
    val runnableST = new RunnableST[(Int, Option[String], Int, Option[String], String, Int, Option[String])] {
      override def apply[S] = for {
        map <- STMap.empty[S, Int, String]
        size0 <- map.size
        get0 <- map.get(0)
        _ <- map += (0 -> "a")
        size1 <- map.size
        get1 <- map.get(0)
        apply0 <- map(0)
        _ <- map -= 0
        size2 <- map.size
        get2 <- map.get(0)
      } yield (size0, get0, size1, get1, apply0, size2, get2)
    }
    val (size0, get0, size1, get1, apply0, size2, get2) = ST.runST(runnableST)
    assert(size0 == 0)
    assert(get0 == None)
    assert(size1 == 1)
    assert(get1 == Option("a"))
    assert(apply0 == "a")
    assert(get2 == None)
  }

  it should "work with STMap.fromMap" in {
    val smap = Map(0 -> "A")
    val runnableST = new RunnableST[(Int, Option[String], Option[String], String, Option[String])] {
      override def apply[S] = for {
        map <- STMap.fromMap[S, Int, String](smap)
        size0 <- map.size
        get0 <- map.get(0)
        _ <- map += (0 -> "a")
        get1 <- map.get(0)
        apply0 <- map(0)
        _ <- map -= 0
        get2 <- map.get(0)
      } yield (size0, get0, get1, apply0, get2)
    }
    val (size0, get0, get1, apply0, get2) = ST.runST(runnableST)
    assert(size0 == 1)
    assert(get0 == Option("A"))
    assert(apply0 == "a")
    assert(get2 == None)
  }

  it should "throw a NoSuchElementException if applied to unknown key" in {
    val runnableST = new RunnableST[String] {
      override def apply[S] = for {
        map <- STMap.empty[S, Int, String]
        apply0 <- map(0)
      } yield apply0
    }
    intercept[NoSuchElementException] {
      ST.runST(runnableST)
    }
  }
}
