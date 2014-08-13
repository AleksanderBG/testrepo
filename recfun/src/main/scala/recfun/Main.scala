package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }m

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
  	@tailrec
  	def calc(n: Int, k: Int, count: Int, acc: Int): Int = {
	  		if(count == 0) acc
	  		else calc(n + 1, k + 1, count - 1, (acc * n) / k)
  	}
  	if(r >= c && c >= 0)
  		if(c > r/2) {
  			def newC = r - c
  			calc(r - newC + 1, 1, newC, 1)
  		}
  		else
  			calc(r - c + 1, 1, c, 1)
  	else
  		0
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
  	@tailrec
  	def openParentheses(chars: List[Char], count: Int): Boolean = {
  		if(chars.isEmpty)
  			count == 0
  		else if(chars.head == '(')
	  		openParentheses(chars.tail, count + 1)
	  	else if(chars.head == ')')
	  		if(count > 0)
					openParentheses(chars.tail, count - 1)
	  		else
	  			false
	  	else
	  		openParentheses(chars.tail, count)
  	}
  	openParentheses(chars, 0)
  }

  /**
   * Exercise 3
   */
	def countChange(money: Int, coins: List[Int]): Int = {
  	if(money == 0) 1
  	else if(money < 0 || coins.isEmpty) 0
  	else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
