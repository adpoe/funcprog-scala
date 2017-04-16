/* 1.1 - code with side effects */
class Cafe {
  def buyCoffee(cc: CreditCard): Coffee = {

    val cup = new Coffee()

    cc.charge(cup.price)

    cup
  }
}

/* 1.2 adding a payments object to increase modularity */

class Cafe {
  def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
    val cup = new Coffee()
    p.charge(cc, cup.price)
    cup
  }
}

/* functional solution */
class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }
}

/* Charge case class */
case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different credit cards")
  }
}

/* buying multiple cups with buyCoffees */
class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = ...

  /* allows us to RE-USE buyCoffee */
  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }
}
