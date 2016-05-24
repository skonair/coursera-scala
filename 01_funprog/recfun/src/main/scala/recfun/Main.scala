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
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
