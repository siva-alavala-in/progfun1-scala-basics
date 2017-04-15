package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("Balance")
    println(balance(")".toList))
    println(countChange(4,List(1, 2)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if (c == 0 || r == c) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def checkBalance(chars: List[Char], open: Int) : Boolean = {
      if (chars.isEmpty) open == 0 else chars.head match {
        case '(' => checkBalance(chars.tail, open+1)
        case ')' => (open > 0) && checkBalance(chars.tail, open-1)
        case _ => checkBalance(chars.tail, open)
      }
    }
    checkBalance(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (_, Nil) => 0
      case (_, List()) => 0
      case (_, _) => (for (i  <- 0 to money/coins.head) yield countChange(money - coins.head * i, coins.tail)).sum
      }
    }
  }
