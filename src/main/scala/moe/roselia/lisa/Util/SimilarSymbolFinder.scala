package moe.roselia.lisa.Util

import moe.roselia.lisa.Environments.Environment

trait SimilarSymbolFinder {
  class Lazy[+T](expr: => T) {
    lazy val value: T = expr
  }
  object Lazy {
    def apply[T](expr: => T): Lazy[T] = new Lazy(expr)
  }
  val addWeight = 1
  val deleteWight = 1
  val editWeight = 2 // When editing something, we must first delete it and then add the correct word.
  /**
   * Calculate the weighed minimum edit distance between two words via the lazy-computing dp.
   * @param word1 The first word.
   * @param word2 The second word.
   * @return The weighed edit distance.
   */
  def editDistanceWeighed(word1: String, word2: String): Int = {
    def genDp(i: Int, j: Int): Lazy[Int] = Lazy {
      if (i == 0 || j == 0) {
        if (i != 0) dp(i - 1)(j).value + 1
        else if(j != 0) dp(i)(j - 1).value + 1
        else 0
      }
      else if(word1.charAt(i - 1) != word2.charAt(j - 1)) {
        val add = dp(i - 1)(j).value + addWeight
        val del = dp(i)(j - 1).value + deleteWight
        val edit = dp(i - 1)(j - 1).value + editWeight
        add.min(del).min(edit)
      }
      else dp(i - 1)(j - 1).value
    }
    lazy val dp = Array.tabulate(word1.length + 1, word2.length + 1)(genDp)
    dp.last.last.value
  }

  def findSuitableSuggestion(variable: String, environment: Environment): Option[String] = {
    val lowerVariable = variable.toLowerCase
    val threshold = Math.round(lowerVariable.length * 0.2).max(editWeight) // We tolerate at least one edit.
    if (variable.length <= 1) { // There is not point correcting symbols with only zero or one character.
      None
    } else {
      environment.collectDefinedValues.map { candidate =>
        (candidate, editDistanceWeighed(lowerVariable, candidate.toLowerCase))
      }.filter { candidate =>
        candidate._2 <= threshold
      }.minByOption(_._2).map(_._1)
    }
  }
}

object SimilarSymbolFinder extends SimilarSymbolFinder
