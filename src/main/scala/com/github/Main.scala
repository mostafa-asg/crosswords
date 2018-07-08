package com.github

import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {
    val crosswordsStr = new StringBuilder

    Range(1,11).foreach{ iteration =>
      crosswordsStr.append(StdIn.readLine())
      crosswordsStr.append("\n")
    }
    val words = StdIn.readLine()
    val crosswords = CrosswordsBuilder.buildFrom(crosswordsStr.toString(), words.split(";").toSet)
    val solution = crosswords.solveAllPossibilities.head.stringRepresentation

    println(solution)
  }

}
