package fpinscala.answers.streamingio

/* 
 * `Eq[A,B]` provides evidence that types `A` and `B` are equal. 
 * There is just one public constructor, `Eq.refl`, ensuring that
 * we cannot construct an `Eq` instance in which the `A` and `B`
 * differ.
 * 
 * There is a version of this in the scala standard library, 
 * called =:=[A,B] (and usually written infix as `A =:= B`) but
 * we include a version here just to show that it is not magic.
 */
case class Eq[A,B] private(to: A => B, from: B => A)

object Eq { def refl[A]: Eq[A,A] = Eq(identity, identity) } 

