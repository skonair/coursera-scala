package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (_, 0) => 1
    case (0, _) => 1
    case (_, _) => if (c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balanceAcc(chars: List[Char], count: Int): Boolean = chars match {
    case ('(' :: as) => balanceAcc(as, count + 1)
    case (')' :: as) => if (count == 0) false else balanceAcc(as, count - 1)
    case (_ :: as)  => balanceAcc(as, count)
    case Nil => count == 0
  }

    def balance(chars: List[Char]): Boolean = balanceAcc(chars, 0)
  
  /**
   * Exercise 3
   */
  def countChangeAcc(money: Int, coins: List[Int], count: Int): Int = coins match {
    case Nil => if (money == 0 && count > 0) 1 else 0
    case (a :: as) => if (money >= 0) countChangeAcc(money - a, coins, count + a) + countChangeAcc(money, as, count) else 0
  }

    def countChange(money: Int, coins: List[Int]): Int = countChangeAcc(money, coins, 0)
  }
