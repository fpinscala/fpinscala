package fpinscala.whatisfp
case class Player(name: String, score: Int) // Declares a data type `Player` with two properties: `name`, which is a string, and `score`, an integer.

object Example {
def printWinner(p: Player): Unit = // Prints the name of the winner to the console.
  println(p.name + " is the winner!")

def winner(p1: Player, p2: Player): Player = // A pure function that takes two players and returns the higher-scoring one.
  if (p1.score > p2.score) p1 else p2 

def declareWinner(p1: Player, p2: Player): Unit =
  printWinner(winner(p1, p2))
val players = List(Player("Sue", 7), // Constructs a list of players
                   Player("Bob", 8),
                   Player("Joe", 4))

val p = players.reduceLeft(winner) // Reduces the list to just the player with the highest score.

printWinner(p) // Prints the name of the winner to the console.
}