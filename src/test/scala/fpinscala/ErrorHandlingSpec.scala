
package fpinscala

import org.scalacheck._
import Prop._

object ErrorHandlingSpec extends Properties("ErrorHandling") {
  
  property("map") = forAll((i: Int) => (
    Some(i) map (_ + 1)       ?= Some(i + 1) ) && (
    None    map ((_:Int) + 1) ?= None        )
  )
 
  property("flatMap") = forAll((i: Int) => (
    Some(i) flatMap (i => Some(i+1)) ?= Some(i) map (_ + 1) ) && (
    None    flatMap (i => Some(10))  ?= None )
  )

  property("getOrElse") = forAll((i: Int) => (
    Some(i) getOrElse sys.error("getOrElse too strict") ?= i ) && (
    None    getOrElse i ?= i )
  )

  property("orElse") = forAll((i: Int, j: Int) => (
    Some(i) orElse None    ?= Some(i) ) && (
    None    orElse Some(j) ?= Some(j) ) && (
    Some(i) orElse Some(j) ?= Some(i) )
  )

  property("filter") = forAll((i: Int) => (
    None    filter ((i: Int) => i%2 == 0) ?= None ) && (
    i%2 == 1 ==> (Some(i) filter (_ % 2 == 0) ?= None) )
  )
}
