package fpinscala.answers.introduction

object FirstExample:

  class Cafe:
    def buyCoffee(cc: CreditCard): Coffee =
      val cup = Coffee()
      cc.charge(cup.price)
      cup

  class CreditCard:
    def charge(price: Double): Unit =
      println("charging " + price)

  class Coffee:
    val price: Double = 2.0

  val cc = CreditCard()
  val cafe = Cafe()
  val cup = cafe.buyCoffee(cc)

object SecondExample:

  class Cafe:
    def buyCoffee(cc: CreditCard, p: Payments): Coffee =
      val cup = Coffee()
      p.charge(cc, cup.price)
      cup

  class CreditCard

  trait Payments:
    def charge(cc: CreditCard, price: Double): Unit

  class SimulatedPayments extends Payments:
    def charge(cc: CreditCard, price: Double): Unit =
      println("charging " + price + " to " + cc)

  class Coffee:
    val price: Double = 2.0

  val cc = CreditCard()
  val p = SimulatedPayments()
  val cafe = Cafe()
  val cup = cafe.buyCoffee(cc, p)

object ThirdExample:

  class Cafe:
    def buyCoffee(cc: CreditCard): (Coffee, Charge) =
      val cup = Coffee()
      (cup, Charge(cc, cup.price))

    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) =
      val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
      val (coffees, charges) = purchases.unzip
      (coffees, charges.reduce((c1, c2) => c1.combine(c2)))

  class CreditCard

  case class Charge(cc: CreditCard, amount: Double):
    def combine(other: Charge): Charge =
      if cc == other.cc then
        Charge(cc, amount + other.amount)
      else
        throw new Exception("Can't combine charges to different cards")

  class Coffee:
    val price: Double = 2.0

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_.combine(_))).toList
