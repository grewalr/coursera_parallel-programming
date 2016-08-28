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
  ) withWarmer new Warmer.Default

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

object ParallelParenthesesBalancing
{

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean =
  {
    def loop(acc: Int, idx: Int): Int =
    {
      if (chars.isEmpty || idx == -1) acc
      else if (idx >= chars.length) chars.length - 1
      else if (chars(idx) == '(') loop(acc + 1, idx + 1)
      else if (chars(idx) == ')') loop(acc - 1, idx + 1)
      else loop(acc, idx + 1)
    }

    loop(0, 0) == 0
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =
  {
    def traverse(idx: Int, until: Int, totOpenBrace: Int, totCloseBrace: Int) : (Int, Int) =
    {
      if (chars.isEmpty || idx >= until) (totOpenBrace, totCloseBrace)
      else if (chars(idx) == '(') traverse(idx + 1, until, totOpenBrace + 1, totCloseBrace)
      else if (chars(idx) == ')') traverse(idx + 1, until, totOpenBrace, totCloseBrace + 1)
      else traverse(idx + 1, until, totOpenBrace, totCloseBrace)
    }

    def reduce(from: Int, until: Int) : (Int, Int) =
    {
      if(until - from < threshold)
      {
        // base case call the traverse function
        val (open, close) = traverse(from, until, 0, 0)
        (open, close)
      }
      else
      {
        val midPoint = from + (until - from) / 2

        val (l, r) = parallel(
          reduce(from, midPoint -1),
          reduce(midPoint, until)
        )

        (l._1 - r._1, l._2 - r._2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
