package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.{Future => JFuture}
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfterEach
import org.scalatest.FlatSpec
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.PropertyChecks

import Par.Par
import Par.asyncF
import Par.choiceMap
import Par.choiceN
import Par.choiceNViaChooser
import Par.choiceViaChoiceN
import Par.choiceViaChooser
import Par.chooser
import Par.flatMapViaJoin
import Par.join
import Par.joinViaFlatMap
import Par.lazyUnit
import Par.parFilter
import Par.sequence

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ParSpec extends FlatSpec with PropertyChecks with BeforeAndAfterEach with Eventually {

  val asyncThreadCount = new AtomicInteger

  private def assertAsync = assert(asyncThreadCount.get > 0, "execution must be async")
  private def assertSync = assert(asyncThreadCount.get == 0, "execution must be sync")

  val threadFactory: ThreadFactory =
    new ThreadFactory {
      override def newThread(r: Runnable) = {
        asyncThreadCount.incrementAndGet
        Executors.defaultThreadFactory.newThread(r)
      }
    }

  var executorService: ExecutorService = _

  override def beforeEach = {
    super.beforeEach
    asyncThreadCount.set(0)
    // note that sequence() will not work if we do not provide enough parallel threads! (see exercise 7.9)
    executorService = Executors.newCachedThreadPool(threadFactory)
  }

  implicit class TestParOps[A](p: Par[A]) {
    def run: JFuture[A] = Par.run(executorService)(p)
    def get: A = Par.run(executorService)(p).get
  }

  behavior of "7.4 asyncF"

  it should "work" in {
    val intToString = (_:Int).toString
    val a = asyncF(intToString)(42).run
    val b = asyncF(intToString)(43).run
    eventually {
      assert(a.get == "42")
      assert(b.get == "43")
      assertAsync
    }
  }

  behavior of "7.5 sequence"

  it should "result in empty List immediately for empty List" in {
    val emptyParIntList = List[Par[Int]]()
    val emptySequence = sequence(emptyParIntList).run
    eventually {
      assert(emptySequence.get == List[Int]())
      assertSync
    }
  }

  it should "work asynchronously for non-empty Lists" in {
    val intPars = List(lazyUnit(1), lazyUnit(2), lazyUnit(3))
    val parListInts = sequence(intPars).run
    eventually {
      assert(parListInts.get == List(1, 2, 3))
      assertAsync
    }
  }

  behavior of "7.6 parFilter"

  it should "result in empty List immediately for empty List" in {
    val emptyParIntList = List[Par[Int]]()
    val emptySequence = parFilter(emptyParIntList)(_ => false).run
    eventually {
      assert(emptySequence.get == List[Int]())
      assertSync
    }
  }

  it should "work asynchronously for non-empty Lists" in {
    val ints = List(1,2,3)
    val parListInts = parFilter(ints)(_ % 2 == 0).run
    eventually {
      assert(parListInts.get == List(2))
      assertAsync
    }
  }

  behavior of "7.11.1 choiceN"

  it should "work asynchronously for non-empty Lists" in {
    val intPars = List(lazyUnit(1), lazyUnit(2), lazyUnit(3))
    val n = lazyUnit(1)
    val parInt = choiceN(n)(intPars).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.11.2 choiceViaChoiceN"

  it should "work asynchronously for true case" in {
    val trueChoice = choiceViaChoiceN(lazyUnit(true))(lazyUnit("yes"), lazyUnit("no")).run
    eventually {
      assert(trueChoice.get == "yes")
      assertAsync
    }
  }

  it should "work asynchronously for false case" in {
    val falseChoice = choiceViaChoiceN(lazyUnit(false))(lazyUnit("yes"), lazyUnit("no")).run
    eventually {
      assert(falseChoice.get == "no")
      assertAsync
    }
  }

  behavior of "7.12 choiceMap"

  it should "work asynchronously" in {
    val choices = Map(1 -> lazyUnit(1), 2 -> lazyUnit(2), 3 -> lazyUnit(3))
    val key = lazyUnit(2)
    val parInt = choiceMap(key)(choices).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.13.1 chooser"

  it should "work asynchronously for List" in {
    val intPars = List(lazyUnit(1), lazyUnit(2), lazyUnit(3))
    val n = lazyUnit(1)
    val parInt = chooser(n)(intPars).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  it should "work asynchronously for Map" in {
    val choices = Map(1 -> lazyUnit(1), 2 -> lazyUnit(2), 3 -> lazyUnit(3))
    val key = lazyUnit(2)
    val parInt = chooser(key)(choices).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.13.2 choiceViaChooser"

  it should "work asynchronously for true case" in {
    val trueChoice = choiceViaChooser(lazyUnit(true))(lazyUnit("yes"), lazyUnit("no")).run
    eventually {
      assert(trueChoice.get == "yes")
      assertAsync
    }
  }

  it should "work asynchronously for false case" in {
    val falseChoice = choiceViaChooser(lazyUnit(false))(lazyUnit("yes"), lazyUnit("no")).run
    eventually {
      assert(falseChoice.get == "no")
      assertAsync
    }
  }

  behavior of "7.13.3 choiceNViaChooser"

  it should "work asynchronously for List" in {
    val intPars = List(lazyUnit(1), lazyUnit(2), lazyUnit(3))
    val n = lazyUnit(1)
    val parInt = choiceNViaChooser(n)(intPars).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.14.1 join"

  it should "work asynchronously" in {
    val ppi = lazyUnit(lazyUnit(42))
    val i = join(ppi)
    eventually {
      assert(i.get == 42)
      assertAsync
    }
  }

  behavior of "7.14.2 flatMapViaJoin"

  it should "work asynchronously" in {
    val pi = lazyUnit(42)
    val i = flatMapViaJoin(pi)(lazyUnit(_))
    eventually {
      assert(i.get == 42)
      assertAsync
    }
  }

  behavior of "7.14.3 joinViaFlatMap"

  it should "work asynchronously" in {
    val ppi = lazyUnit(lazyUnit(42))
    val i = joinViaFlatMap(ppi)
    eventually {
      assert(i.get == 42)
      assertAsync
    }
  }
}