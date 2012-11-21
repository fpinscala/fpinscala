package fpinscala.datastructures

import org.specs2.mutable._

import List._ // implementations

object ListSpec extends Specification {
  "List specification" title

  "List tail" should { // ex 2
    "be Nil for a list with a single element list" in {
      tail(singleList) aka "tail" mustEqual Nil
    }

    "be [1]" in {
      tail(Cons(0, singleList)) aka "tail" mustEqual singleList
    }
  }

  "Dropping list element(s)" should { // ex 3
    "return Nil from a single element list" in {
      drop(singleList, 1).
        aka("list with first element dropped") mustEqual (Nil)
    }

    "return [1] from a 2 elements list" in {
      drop(Cons(0, singleList), 1).
        aka("list with first element dropped") mustEqual (singleList)
    }

    "return [1] from a 3 elements list" in {
      drop(Cons(3, Cons(2, singleList)), 2).
        aka("list with 2 elements dropped") mustEqual singleList

    }
  }

  "Dropping list element(s) based on predicate" should { // ex 4
    val phrase: List[Char] =
      List('f', 'i', 'r', 's', 't', // first
        ' ', // space
        'w', 'o', 'r', 'd')

    val sndWord = List('w', 'o', 'r', 'd')

    """on character list "first word"""" >> {
      """return "word" from removing until a space on is found""" in {
        tail( // just remove ' ' token from second word
          dropWhile(phrase)(_ != ' ')) aka "last word" mustEqual sndWord
      }
    }
  }

  "Setting list head" should { // ex 5
    "on [1] with 2 return" in {
      setHead(singleList)(2) aka "new list" mustEqual Cons(2, Nil)
    }
  }

  "Initial part" should { // ex 6
    "for [1,2,3,4] return [1,2,3]" in {
      init(List(1, 2, 3, 4)) aka "initial part" mustEqual List(1, 2, 3)
    }
  }

  "List length" should { // ex 10 & 12
    "for Nil be 0" in {
      fpinscala.datastructures.List.length(Nil) aka "list length" mustEqual 0
    }

    "for [1] be 1" in {
      fpinscala.datastructures.List.
        length(singleList) aka "list length" mustEqual 1
    }

    "for [1,2,3] be 3" in {
      fpinscala.datastructures.List.
        length(List(1, 2, 3)) aka "list length" mustEqual 3
    }
  }

  "Appending element" should { // ex 15
    "with 1 on Nil return [1]" in {
      fpinscala.datastructures.List.
        append(Nil: List[Int], singleList) aka "new list" mustEqual singleList
    }

    "with 2 on [1] return [1,2]" in {
      fpinscala.datastructures.List.
        append(singleList, Cons(2, Nil)).
        aka("new list") mustEqual Cons(1, Cons(2, Nil))
    }

    "with [3,4] on [1,2] return [1,2]" in {
      fpinscala.datastructures.List.
        append(Cons(1, Cons(2, Nil)), Cons(3, Cons(4, Nil))).
        aka("new list") mustEqual Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    }
  }

  "Concatenating list of list" should { // ex 16
    val a = Cons(1, Cons(2, Nil))
    val b = Cons(3, Cons(4, Nil))

    "on [[1,2],[3,4]] return [1,2,3,4]" in {
      fpinscala.datastructures.List.
        concat(Cons(a, Cons(b, Nil))).
        aka("new list") mustEqual Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    }
  }

  "Adding 1" should { // ex 17
    "on [1] return [2]" in {
      add1(Cons(1, Nil)) aka "new list" mustEqual Cons(2, Nil)
    }

    "on [2,3] return [3,4]" in {
      add1(Cons(2, Cons(3, Nil))) aka "new list" mustEqual Cons(3, Cons(4, Nil))
    }
  }

  "List of double [1,2]" should { // ex 18
    "be turned in a list of string" in {
      val l: List[String] = doubleToString(List(1d, 2d))
      val compiled = true

      compiled aka "compilation of `l`" must beTrue
    }
  }

  """List ["first", "word"]""" should { // ex 19
    "have its elements modified while keeping structure using `map`" in {
      fpinscala.datastructures.List.
        map(Cons("first", Cons("word", Nil))) { _.length }.
        aka("list of string lengths") mustEqual List(5, 4)
    }
  }

  "List [0,0,1,0,2,0,0,3] filter" should { // ex 20
    "be filtered to [1,2,3]" in {
      filter(List(0, 0, 1, 0, 2, 0, 0, 3)) { _ > 0 }.
        aka("filtered list") mustEqual List(1, 2, 3)
    }
  }

  "List of string" should {
    "be flat-mapped to list of list of characters" in {
      fpinscala.datastructures.List.
        flatMap(List("first", "word")) { s ⇒ List(s.toArray: _*) }.
        aka("new list") mustEqual List('f', 'i', 'r', 's', 't',
          'w', 'o', 'r', 'd')
    }
  }

  "Lists [1,2,3] and [4,5,6]" should {
    "be added pairwisely" in {
      addPairwise(List(1, 2, 3), List(4, 5, 6)).
        aka("new list") mustEqual List(5, 7, 9)
    }
  }

  """Lists ["one","the"] and ["is","first"]""" should {
    """be zipped togather as ["one is", "the first"]""" in {
      zipWith(List("one", "the"), List("is", "first")) {
        (a, b) ⇒ a + " " + b
      } aka ("zipped list") mustEqual List("one is", "the first")
    }
  }

  // ---

  val singleList = Cons(1, Nil)
}
