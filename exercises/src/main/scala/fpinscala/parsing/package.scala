package fpinscala

package object parsing {
  type Parser[+A] = Location => Either[ParseError, (A, Location)]
}
