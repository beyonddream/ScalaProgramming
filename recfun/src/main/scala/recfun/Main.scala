package recfun

import scala.annotation.tailrec

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
    def pascal(c: Int, r: Int): Int = {

      val accumulator: Array[Array[Int]] = Array.fill[Int](r + 1, c + 1)(-1)

      pascalInternal(c, r, accumulator)

      def pascalInternal(currentColumn: Int, currentRow: Int, accumulator: Array[Array[Int]]): Int = {

        if (currentColumn < 0 || currentRow < 0)
          0
        else if (currentColumn == 0 && currentRow == 0)
          1
        else if (accumulator(currentRow)(currentColumn) != -1)
          accumulator(currentRow)(currentColumn)
        else {
          accumulator(currentRow)(currentColumn) = pascalInternal(currentColumn - 1, currentRow - 1, accumulator) +
            pascalInternal(currentColumn, currentRow - 1, accumulator)
          accumulator(currentRow)(currentColumn)
        }
      }

      accumulator(r)(c)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      val accumulator: List[Char] = List()

      @tailrec
      def balanceInternal(chars: List[Char], acc: List[Char]): Boolean = {
        if (chars.isEmpty)
          acc.isEmpty
        else if (chars.head == ')' && (acc.isEmpty || acc.head != '('))
         false
        else
          balanceInternal(chars.tail,
            if (chars.head == '(') acc.::(chars.head) else (if (chars.head == ')') acc.tail else acc))
      }

      balanceInternal(chars, accumulator) && accumulator.isEmpty
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeInternal(money: Int, coins: List[Int]): Int = {
        if (money < 0)
          0
        else if (money == 0)
          1
        else if (coins.isEmpty)
          0
        else
          countChangeInternal(money - coins.head, coins) +
            countChangeInternal(money, coins.tail)
      }

      countChangeInternal(money, coins)
    }
  }
