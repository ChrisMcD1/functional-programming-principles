package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || r == 0 || r == c then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = balancePair(chars, 0);

  def balancePair(chars: List[Char], count: Int): Boolean =
    if count < 0 then false
    else if chars.isEmpty then count == 0
    else balancePair(chars.tail, count + charToCount(chars.head))

  def charToCount(char: Char): Int =
    if char == '(' then 1
    else if char == ')' then -1
    else 0

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then 0
    else if coins.isEmpty then 0
    else countChangeHelper(money, coins.sorted(Ordering.Int.reverse))

  def countChangeHelper(money: Int, coins: List[Int]): Int =
    if money == 0 then 1
    else if money < 0 then 0
    else if coins.isEmpty then 0
    else
      countChangeHelper(money - coins.head, coins) + countChangeHelper(
        money,
        coins.tail
      )
