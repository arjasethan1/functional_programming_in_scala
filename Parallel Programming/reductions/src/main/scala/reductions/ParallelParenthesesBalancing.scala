package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */


    def balance(chars: Array[Char]): Boolean = {
      def balanceHelp(chars : Array[Char], open: Int): Boolean ={
        if(chars.isEmpty) open == 0
        else if(chars.head == '(') balanceHelp(chars.tail, open+1)
        else if(chars.head == ')') balanceHelp(chars.tail, open -1) && open > 0
        else balanceHelp(chars.tail, open)
      }
      balanceHelp(chars, 0)
    }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */


  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      def traverseHelp(chars : Array[Char], arg1: Int, arg2 :Int) : (Int, Int) = {
      if(chars.isEmpty) (arg1, arg2)
      else if(chars.head == '(') traverseHelp(chars.tail, arg1 +1, arg2)
      else if(chars.head == ')') {
        if(arg1 > 0) traverseHelp(chars.tail, arg1-1, arg2)
        else traverseHelp(chars.tail, arg1, arg2)
      }
      else (traverseHelp(chars.tail, arg1, arg2))
      }
      traverseHelp(chars.slice(idx, until), 0, 0)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(from - until < threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (pair1, pair2) = parallel(reduce(from, mid), reduce(mid, until))


        if(pair1._1 < 0 && pair2._1 > 0) (pair1._1 , pair2._1 + pair1._2 + pair2._2)
        else if(pair2._1 < 0 && pair1._2 > 0) (pair1._1 + pair2._1 + pair1._2 ,  + pair2._2)
        else (pair1._1 + pair2._1, pair1._2 + pair2._2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
