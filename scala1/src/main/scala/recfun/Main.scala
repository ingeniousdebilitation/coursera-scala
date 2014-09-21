package recfun
import common._

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
   * 
   * Vzorce pro kombinační čísla.
   */
  def pascal(c: Int, r: Int): Int = 
    if(r < c) 0 
    else if(r == c || c == 0) 1
	else pascal(c-1,r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   * 
   * Když došly symboly na "pásce", tak TRUE když je počet otevírajících 
   * závorek (count) roven nule, tedy stejný počet otevírajícíh a zavírajících.
   * 
   * Když je načtena zavírající závorka a nebyl předem načten dostatečný počet
   * otevírajících závorek (count je 0 a míň), tak FALSE.
   * 
   * Když je načtena zavírající závorka a byl předem načten dostatečný počet
   * otevírajících závorek, tak iterovat pro zbytek a snížit počet otevírajících
   * závorek o 1.
   * 
   * Když je načtena otevírající závorka tak iterovat pro zbytek a zvýšit count o 1.
   * 
   * Pokud je načten jiný symbol, tak iterovat beze změny count.
   */
	def balance(chars: List[Char]): Boolean = {
		def iter(chars: List[Char], count: Int): Boolean =
			if(chars.isEmpty) count==0
			else if(chars.head.equals(')') && count <= 0) false
			else if(chars.head.equals(')') && count > 0) iter(chars.tail, count-1)
			else if(chars.head.equals('(')) iter(chars.tail, count+1)
			else iter(chars.tail, count)
		iter(chars, 0);
	}
  /**
   * Exercise 3
   * 
   * Když je money měnší něž nula, tak žádný výběr.
   * 
   * Pokud je money rovno nule, tak má jeden způsob výběru -- nevyberu nic.
   *
   * A v posledním případě se množina možných výběrů rozpadne na dvě podmnožiny:
   * Pokud jsem prvek a1 vybral, tak provádím výběr pro money-a1 a všech mincí (mince se může opakovat) 
   * 		-- countChange(money - coins.head, coins)
   * Pokud nevyberu prvek a1 (money zůstává nezměněn) a můžu vybírat ze zbytku mincí (tu minci jsem nevybral)
   * 		-- countChange(money, coins.tail)
   *   
   * (viz KME Pascalova identita)
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if(money < 0 || coins.isEmpty) 0
    else if(money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
