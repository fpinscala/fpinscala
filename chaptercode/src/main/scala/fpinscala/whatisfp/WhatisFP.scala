package fpinscala.whatisfp
case class Player(name: String, score: Int) // Declares a data type `Player` with two properties: `name`, which is a string, and `score`, an integer.

object Example {
def declareWinner(p: Player): Unit = // Prints the name of the winner to the console.
  println(p.name + " is the winner!")

def maxScore(p1: Player, p2: Player): Player = // A pure function that takes two players and returns the higher-scoring one.
  if (p1.score > p2.score) p1 else p2 

def winner(p1: Player, p2: Player): Unit =
  declareWinner(maxScore(p1, p2))
val players = List(Player("Bob", 7), // Constructs a list of players
                   Player("Sue", 8),
                   Player("Joe", 4))

val highScore = players.reduceLeft(maxScore) // Reduces the list to just the player with the highest score.

declareWinner(highScore) // Prints the name of the winner to the console.
}