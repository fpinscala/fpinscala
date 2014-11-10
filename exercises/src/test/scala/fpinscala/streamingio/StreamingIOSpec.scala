package fpinscala.streamingio

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import SimpleStreamTransducers.{ Process => SIProcess }
import GeneralizedStreamTransducers.Process
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import fpinscala.iomonad.Task
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StreamingIOSpec extends FlatSpec with PropertyChecks {

  private def between0AndN(n: Int) = Gen.chooseNum(0, n) label "n"

  behavior of "15.1.1 SIProcess.take"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll(between0AndN(l.size)) { n: Int =>
        val result = SIProcess.take(n)(l.toStream)
        assert(result.toList == l.take(n))
      }
    }
  }

  behavior of "15.1.2 SIProcess.drop"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll(between0AndN(l.size)) { n: Int =>
        val result = SIProcess.drop(n)(l.toStream)
        assert(result.toList == l.drop(n))
      }
    }
  }

  private def even(i: Int) = (i % 2) == 0

  behavior of "15.1.3 SIProcess.takeWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SIProcess.takeWhile(even)(l.toStream)
      assert(result.toList == l.takeWhile(even))
    }
  }

  behavior of "15.1.4 SIProcess.dropWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SIProcess.dropWhile(even)(l.toStream)
      assert(result.toList == l.dropWhile(even))
    }
  }

  behavior of "15.2 SIProcess.count"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SIProcess.count(l.toStream)
      assert(result.toList == l.zipWithIndex.map(_._2 + 1))
    }
  }

  private def listOfMeans(doubles: List[Double]) = {
    val (reversedMeans, _, _) = doubles.foldLeft((List[Double](), 0d, 0)) {
      case ((means, sum, index), d) =>
        (((sum + d) / (index + 1)) :: means, (sum + d), index + 1)
    }
    reversedMeans.reverse
  }

  behavior of "15.3 SIProcess.mean"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = SIProcess.mean(doubles.toStream)
      assert(result.toList == listOfMeans(doubles))
    }
  }

  behavior of "15.4.1 SIProcess.sumViaLoop"
  it should "work" in {
    def listOfSums(doubles: List[Double]) = {
      val (reversedSums, _) = doubles.foldLeft((List[Double](), 0d)) {
        case ((sums, sum), d) => ((sum + d) :: sums, sum + d)
      }
      reversedSums.reverse
    }
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = SIProcess.sumViaLoop(doubles.toStream)
      assert(result.toList == listOfSums(doubles))
    }
  }

  behavior of "15.4.2 SIProcess.countViaLoop"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SIProcess.countViaLoop(l.toStream)
      assert(result.toList == l.zipWithIndex.map(_._2 + 1))
    }
  }

  behavior of "15.5 SIProcess.|>"
  it should "work" in {
    val evenProcess = SIProcess.filter(even)
    val toStringProcess = SIProcess.lift((_: Int).toString)
    val pipe = evenProcess |> toStringProcess
    forAll("l") { l: List[Int] =>
      val result = pipe(l.toStream)
      assert(result.toList == l.filter(even).map(_.toString))
    }
  }

  behavior of "15.6 SIProcess.zipWithIndex"
  it should "work in object Process" in {
    forAll("l") { l: List[Int] =>
      val result = SIProcess.zipWithIndex(SIProcess.id[Int])(l.toStream)
      assert(result.toList == l.zipWithIndex)
    }
  }
  it should "work in trait Process" in {
    forAll("l") { l: List[Int] =>
      val result = SIProcess.id[Int].zipWithIndex(l.toStream)
      assert(result.toList == l.zipWithIndex)
    }
  }

  behavior of "15.7.1 SIProcess.zip"
  it should "work" in {
    val plus1 = (_: Int) + 1
    val plus1Process = SIProcess.lift(plus1)
    forAll("l") { l: List[Int] =>
      val result = SIProcess.zip(SIProcess.id[Int], plus1Process)(l.toStream)
      assert(result.toList == l.zip(l map plus1))
    }
  }

  behavior of "15.7.2 SIProcess.meanViaZip"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = SIProcess.meanViaZip(doubles.toStream)
      assert(result.toList == listOfMeans(doubles))
    }
  }

  behavior of "15.8 SIProcess.exists"
  it should "work" in {
    val isZero = (_: Int) == 0
    def existsResult(l: List[Int]): List[Boolean] =
      l.foldLeft((List[Boolean](), false)) {
        case ((lb, foundZero), i) =>
          ((foundZero || isZero(i)) :: lb, foundZero || isZero(i))
      }._1.reverse
    forAll("l") { l: List[Int] =>
      val result = SIProcess.exists(isZero)(l.toStream)
      assert(result.toList == existsResult(l))
    }
  }

  behavior of "15.9 SIProcess.convertFahrenheit"
  it should "work" in {
    implicit val arbLine: Arbitrary[String] = {
      val whitespace = Gen.oneOf(" ", "\t")
      val fahrenheit = Gen.chooseNum(-100d, 200d)
      val blankLine = Gen.listOf(whitespace).map(_.mkString)
      val commentLine = Gen.const("# no comment!")
      val celsiusLine = fahrenheit map (_.toString)
      Arbitrary(Gen.frequency((3, celsiusLine), (1, commentLine), (1, blankLine)))
    }
    forAll("line") { lines: List[String] =>
      val result = SIProcess.convertFahrenheit(lines.toStream)
      val celsiusLines =
        lines.filter(!_.trim.isEmpty)
          .filter(!_.startsWith("#"))
          .map(line => SIProcess.toCelsius(line.trim.toDouble).toString)
      assert(result.toList == celsiusLines)
    }
  }

  import Process._

  private def listTaskProcess(ls: => List[Int]): Process[Task, Int] = {
    await(Task.unit(ls)) {
      case Left(e) => Halt(e)
      case Right(l) =>
        lazy val next: Process[Task, Int] =
          if (l.isEmpty) Halt(End)
          else emit(l.head, listTaskProcess(ls.tail))
        next
    }
  }

  private def withExecutionService[A](block: ExecutorService => A) {
    val es = Executors.newCachedThreadPool
    try block(es) finally es.shutdown
  }

  behavior of "15.10 Process.runLog"
  it should "work for normal cases" in {
    withExecutionService { implicit es: ExecutorService =>
      forAll("l") { l: List[Int] =>
        val p: Process[Task,Int] = listTaskProcess(l)
        val task: Task[IndexedSeq[Int]] = p.runLog
        val Right(result) = task.attemptRun
        assert(result.toList == l)
      }
    }
  }

//  it should "work for Exceptions" in {
//    val exception = new Exception("oops")
//    lazy val l: List[Int] = List(1, 2, throw exception, 4)
//    withExecutionService { implicit es: ExecutorService =>
//        val p = listTaskProcess(l)
//        val task = p.runLog
//        val Left(e) = task.attemptRun
//        assert(e == exception)
//    }
//  }

  private implicit def optionCatch = new MonadCatch[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](a: Option[A])(f: A => Option[B]): Option[B] = a flatMap f
    def attempt[A](oa: Option[A]): Option[Either[Throwable,A]] = oa match {
      case Some(a) => Some(Right(a))
      case _ => None
    }
    def fail[A](err: Throwable): Option[A] = None
  }

  behavior of "15.11.1 Process.eval"
  it should "work" in {
    forAll("oi") { oi: Option[Int] =>
      val p: Process[Option,Int] = eval(oi)
      val result: Option[IndexedSeq[Int]] = p.runLog
      val or: Option[Int] = result map {_(0)}
      assert(or == oi)
    }
  }

  behavior of "15.11.2 Process.eval_"
  it should "work" in {
    forAll("oi") { oi: Option[Int] =>
      val p: Process[Option,String] = eval_(oi)
      val result: Option[IndexedSeq[String]] = p.runLog
      assert(result == oi.map(_ => IndexedSeq()))
    }
  }

  behavior of "15.12 Process.join"
  it should "work" in {
    forAll("oi") { oi: Option[Int] =>
      val ppoi: Process[Option,Process[Option,Int]] = eval(Some(eval(oi)))
      val p: Process[Option,Int] = join(ppoi)
      val result: Option[IndexedSeq[Int]] = p.runLog
      val or: Option[Int] = result map {_(0)}
      assert(or == oi)
    }
  }
}
