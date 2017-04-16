package chapter6

/**
 * Created by kzhang on 1/20/15.
 */

import chapter6.State._

trait State[ S, +A ] {

  def apply ( initial : S ) : (S, A)

  def flatMap[ B ] ( f : A => State[ S, B ] ) : State[ S, B ] = State {
    apply ( _ ) match {
      case (s, a) => f ( a ).apply ( s )
    }
  }

  def map[ B ] ( f : A => B ) : State[ S, B ] = flatMap ( a => unit ( f ( a ) ) )

  def map2[ B, C ] ( rb : State[ S, B ] )( f : (A, B) => C ) : State[ S, C ] = for {
    a <- this
    b <- rb
  } yield f ( a, b )

  def get[ S ] : State[ S, S ] = State ( s => (s, s) )

  def set[ S ] ( s : S ) : State[ S, Unit ] = State ( _ => (s, ()) )
}

object State {
  def apply[ S, A ] ( f : S => (S, A) ) : State[ S, A ] = new State[ S, A ] {
    def apply ( initial : S ) = f ( initial )
  }

  def unit[ S, A ] ( a : A ) : State[ S, A ] = State { (_, a) }


  def sequence[ S, A ] ( fs : List[ State[ S, A ] ] ) : State[ S, List[ A ] ] =
    fs.reverse.foldLeft ( unit[ S, List[ A ] ]( Nil ) )(
      ( sum, last ) => last.map2 ( sum )( ( a, b ) => a :: b )
    )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine ( inputs : List[ Input ] ) : State[ Machine, (Int, Int) ] = State{
    s=> ???
  }
}