package fpinscala.state

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import RNG._
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StateSpec extends FlatSpec with PropertyChecks with Matchers {

  private case class TestRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newValue = if (value == Int.MaxValue) Int.MinValue else value + 1
      (value, TestRNG(newValue))
    }
  }

  private def testCornerCases(test: Int => Unit) = {
    val tests = Table(
      ("n: Int"),
      (0), (1), (-1), (Int.MaxValue), (Int.MinValue))
    forAll(tests)(test)
  }

  private def testProperty(test: Int => Unit) = {
    forAll("n: Int") { n: Int =>
      test(n)
    }
  }

  behavior of "6.1 nonNegativeInt"

  def testNonNegativeInt(n: Int) = nonNegativeInt(TestRNG(n))._1 should be >= 0

  it should "work for corner cases" in {
    testCornerCases(testNonNegativeInt)
  }

  it should "return a value >= 0 for all nextInt values" in {
    testCornerCases(testNonNegativeInt)
  }

  behavior of "6.2 double"

  def testDoubleRange(d: Double) = d should (be >= 0d and be < 1d)

  def testDouble(n: Int) = testDoubleRange(double(TestRNG(n))._1)

  it should "work for corner cases" in {
    testCornerCases(testDouble)
  }

  it should "return a value >= 0 and < 1 for all nextInt values" in {
    testCornerCases(testDouble)
  }

  behavior of "6.3.1 intDouble"

  def testIntDouble(n: Int) = {
    val ((i, d), _) = intDouble(TestRNG(n))
    testDoubleRange(d)
  }

  it should "work for corner cases" in {
    testCornerCases(testIntDouble)
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    testProperty(testIntDouble)
  }

  behavior of "6.3.2 doubleInt"

  def testDoubleInt(n: Int) = {
    val ((d, i), _) = doubleInt(TestRNG(n))
    testDoubleRange(d)
  }

  it should "work for corner cases" in {
    testCornerCases(testDoubleInt)
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    testProperty(testDoubleInt)
  }

  behavior of "6.3.3 double3"

  def testDouble3(n: Int) = {
    val ((d1, d2, d3), _) = double3(TestRNG(n))
    testDoubleRange(d1); testDoubleRange(d2); testDoubleRange(d3)
  }

  it should "work for corner cases" in {
    testCornerCases(testDouble3)
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    testProperty(testDouble3)
  }

  behavior of "6.4 ints"

  def testInts(count: Int)(n: Int) = {
    val (is, _) = ints(count)(TestRNG(n))
    is should have length count
  }

  it should "work for corner cases" in {
    testCornerCases(testInts(0))
    testCornerCases(testInts(1))
    testCornerCases(testInts(10))
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    forAll(Gen.chooseNum(0, 1) :| "count: Int") { count: Int =>
      testProperty(testInts(count))
    }
  }

  behavior of "6.5 doubleViaMap"

  def testDoubleViaMap(n: Int) = testDoubleRange(doubleViaMap(TestRNG(n))._1)

  it should "work for corner cases" in {
    testCornerCases(testDoubleViaMap)
  }

  it should "return a value >= 0 and < 1 for all nextInt values" in {
    testCornerCases(testDoubleViaMap)
  }

  behavior of "6.6 map2"

  def testMap2(n: Int) = {
    val (d, _) = map2(double, double)(_ * _)(TestRNG(n))
    testDoubleRange(d)
  }

  it should "work for corner cases" in {
    testCornerCases(testMap2)
  }

  it should "return a value >= 0 and < 1 for all nextInt values" in {
    testCornerCases(testMap2)
  }

  behavior of "6.7 sequence"

  def testSequence(count: Int)(n: Int) = {
    val seq = List.fill(count)(int)
    val (is, _) = sequence(seq)(TestRNG(n))
    is should have length count
  }

  it should "work for corner cases" in {
    testCornerCases(testSequence(0))
    testCornerCases(testSequence(1))
    testCornerCases(testSequence(10))
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    forAll(Gen.chooseNum(0, 1) :| "count: Int") { count: Int =>
      testProperty(testSequence(count))
    }
  }

}
