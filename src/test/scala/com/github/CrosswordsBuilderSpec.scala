package com.github

import org.scalatest.FlatSpec

class CrosswordsBuilderSpec extends FlatSpec {

  import StringUtil._

  "test 1" should "build crosswords from string representation" in {

    // crosswordsStr looks like this
    //++++++++++
    //+++-----++
    //+++-++++++
    //+++-++++++
    //++++++++++
    //++++++++++
    //++++++++++
    //++++++++++
    //++++++++++
    //++++++++++

    var crosswordsStr = "++++++++++\n+++-----++\n+++-++++++\n+++-++++++\n"
    crosswordsStr += "++++++++++\n".times(6)

    val crosswords = CrosswordsBuilder.buildFrom(crosswordsStr, Set("ABCDE","NOT","AIR","XYZWR"))

    // Placeholders
    assert(crosswords.placeholders.size == 2)
    assert(crosswords.placeholders.contains(
      Placeholder.horizontal("H1-3", 5, Point(1,3), Set("ABCDE", "XYZWR"))))
    assert(crosswords.placeholders.contains(
      Placeholder.vertical("V1-3", 3, Point(1,3), Set("AIR", "NOT"))))

    // Solutions
    assert( crosswords.solveAllPossibilities.size == 1 )
  }

  "test 2" should "build crosswords from string representation" in {

    // crosswordsStr looks like this
    //+-++++++++
    //+-++++++++
    //+-++++++++
    //+-----++++
    //+-+++-++++
    //+-+++-++++
    //+++++-++++
    //++------++
    //+++++-++++
    //+++++-++++

    var crosswordsStr = "+-++++++++\n".times(3)
    crosswordsStr += "+-----++++\n+-+++-++++\n+-+++-++++\n+++++-++++\n++------++\n"
    crosswordsStr += "+++++-++++\n".times(2)

    val crosswords = CrosswordsBuilder.buildFrom(
      crosswordsStr, Set("LONDON","DELHI","ICELAND","ANKARA"))

    // Placeholders
    assert(crosswords.placeholders.size == 4)
    assert(crosswords.placeholders.contains(
      Placeholder.horizontal("H3-1", 5, Point(3,1), Set("DELHI"))))
    assert(crosswords.placeholders.contains(
      Placeholder.horizontal("H7-2", 6, Point(7,2), Set("LONDON", "ANKARA"))))
    assert(crosswords.placeholders.contains(
      Placeholder.vertical("V0-1", 6, Point(0,1), Set("LONDON", "ANKARA"))))
    assert(crosswords.placeholders.contains(
      Placeholder.vertical("V3-5", 7, Point(3,5), Set("ICELAND"))))

    // Solutions
    assert( crosswords.solveAllPossibilities.size == 1 )
  }

  "test 3" should "build crosswords from string representation" in {

    // crosswordsStr looks like this
    //+-++++++++
    //+-++++++++
    //+-------++
    //+-++++++++
    //+-++++++++
    //+------+++
    //+-+++-++++
    //+++++-++++
    //+++++-++++
    //++++++++++

    var crosswordsStr = "+-++++++++\n".times(2)
    crosswordsStr += "+-------++\n" + "+-++++++++\n".times(2)
    crosswordsStr += "+------+++\n" + "+-+++-++++\n" + "+++++-++++\n".times(2)
    crosswordsStr += "++++++++++\n"

    val crosswords = CrosswordsBuilder.buildFrom(
      crosswordsStr, Set("AGRA", "NORWAY", "ENGLAND", "GWALIOR"))

    // Placeholders
    assert(crosswords.placeholders.size == 4)
    assert(crosswords.placeholders.contains(
      Placeholder.horizontal("H2-1", 7, Point(2,1), Set("ENGLAND", "GWALIOR"))))
    assert(crosswords.placeholders.contains(
      Placeholder.horizontal("H5-1", 6, Point(5,1), Set("NORWAY"))))
    assert(crosswords.placeholders.contains(
      Placeholder.vertical("V0-1", 7, Point(0,1), Set("ENGLAND", "GWALIOR"))))
    assert(crosswords.placeholders.contains(
      Placeholder.vertical("V5-5", 4, Point(5,5), Set("AGRA"))))

    // Solutions
    assert( crosswords.solveAllPossibilities.size == 1 )
  }

}