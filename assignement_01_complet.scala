package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }                                               //> main: (args: Array[String])Unit

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c,r) match {
     case (0, _) => 1
     case (c, r)  if (c==r) => 1
     case (c,r) => pascal(c,r-1)+pascal(c-1,r-1)
  }                                               //> pascal: (c: Int, r: Int)Int
  pascal(1,1)                                     //> res0: Int = 1

  /**
   * Exercise 2
   */
  def balance(chars: List[Char], count:Int = 0): Boolean = (chars, count) match {
     case (cs, 0)  if cs.isEmpty => true
     case (cs, _)  if cs.isEmpty => false
     case (cs, c)  => cs.head match {
        case '(' => balance(cs.tail, c+1)
        case ')' if c > 0 => balance(cs.tail, c-1)
        case ')' => false
        case _  => balance(cs.tail, c)
     }
  }                                               //> balance: (chars: List[Char], count: Int)Boolean
  balance(List('a', '(', 'p',')'),0)              //> res1: Boolean = true

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
     case (0, _) => 1
     case (m, _) if m < 0 => 0
     case (_, cs)  if cs.isEmpty => 0
     case (m, cs) => countChange(m - cs.head, cs) + countChange(m, cs.tail)
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  countChange(2,List(1,1))                        //> res2: Int = 3
}