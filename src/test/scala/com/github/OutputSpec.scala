package com.github

import org.scalatest.FlatSpec
import StringUtil._

class OutputSpec extends FlatSpec {

  "Render empty value placeholders" should "print - in the placeholder space" in {

    val ph1 = Placeholder.horizontal(
      id = "h1", size = 5, startPosition = Point(4,3), candidateValues = Set("TRACK"))

    val ph2 = Placeholder.vertical(
      id = "v1", size = 4, startPosition = Point(3,5), candidateValues = Set("MALE"))

    val crosswords = Crosswords(10, 10 , List(ph1, ph2))

    val output = crosswords.render

    // expected output
    //++++++++++
    //++++++++++
    //++++++++++
    //+++++-++++
    //+++-----++
    //+++++-++++
    //+++++-++++
    //++++++++++
    //++++++++++
    //++++++++++
    var expectedOutput = "++++++++++\n".times(3)
    expectedOutput += "+++++-++++\n+++-----++\n"
    expectedOutput += "+++++-++++\n".times(2) + "++++++++++\n".times(3)

    assert(output.equals(expectedOutput))
  }

  "Render solved crosswords" should "print placeholder's value" in {

    val ph1 = Placeholder.horizontal(
      id = "h1", size = 5, startPosition = Point(4,3), candidateValues = Set("TRACK"))

    val ph2 = Placeholder.vertical(
      id = "v1", size = 4, startPosition = Point(3,5), candidateValues = Set("MALE"))

    val crosswords = Crosswords(10, 10 , List(ph1, ph2))
    assert(crosswords.solveAllPossibilities.size == 1)

    val output = crosswords.solveAllPossibilities.head.render

    // expected output
    //++++++++++
    //++++++++++
    //++++++++++
    //+++++M++++
    //+++TRACK++
    //+++++L++++
    //+++++E++++
    //++++++++++
    //++++++++++
    //++++++++++
    var expectedOutput = "++++++++++\n".times(3)
    expectedOutput += "+++++M++++\n+++TRACK++\n"
    expectedOutput += "+++++L++++\n" + "+++++E++++\n" + "++++++++++\n".times(3)

    assert(output.equals(expectedOutput))
  }

  "Crosswords" should "solve all possibilities" in {
    val ph1 = Placeholder.horizontal(
      id = "h1", size = 5, startPosition = Point(4,3), candidateValues = Set("TRACK","ACTOR"))

    val ph2 = Placeholder.vertical(
      id = "v1", size = 4, startPosition = Point(3,5), candidateValues = Set("MALE","EARN","MOON","ATTR"))

    val crosswords = Crosswords(10, 10 , List(ph1, ph2))
    crosswords.addIntersection(Intersection(from = (ph1.id,2), to = (ph2.id,1)))
    assert(crosswords.solveAllPossibilities.size == 3)
    val outputs = crosswords.solveAllPossibilities.map(_.render)

    // Expected output
    //++++++++++
    //++++++++++
    //++++++++++
    //+++++M++++
    //+++TRACK++
    //+++++L++++
    //+++++E++++
    //++++++++++
    //++++++++++
    //++++++++++

    //++++++++++
    //++++++++++
    //++++++++++
    //+++++E++++
    //+++TRACK++
    //+++++R++++
    //+++++N++++
    //++++++++++
    //++++++++++
    //++++++++++

    //++++++++++
    //++++++++++
    //++++++++++
    //+++++A++++
    //+++ACTOR++
    //+++++T++++
    //+++++R++++
    //++++++++++
    //++++++++++
    //++++++++++

    var trackMaleOutput = "++++++++++\n".times(3)
    trackMaleOutput += "+++++M++++\n+++TRACK++\n"
    trackMaleOutput += "+++++L++++\n" + "+++++E++++\n" + "++++++++++\n".times(3)

    var trackEarnOutput = "++++++++++\n".times(3)
    trackEarnOutput += "+++++E++++\n+++TRACK++\n"
    trackEarnOutput += "+++++R++++\n" + "+++++N++++\n" + "++++++++++\n".times(3)

    var actorAttrOutput = "++++++++++\n".times(3)
    actorAttrOutput += "+++++A++++\n+++ACTOR++\n"
    actorAttrOutput += "+++++T++++\n" + "+++++R++++\n" + "++++++++++\n".times(3)

    assert(outputs.contains(trackMaleOutput))
    assert(outputs.contains(trackEarnOutput))
    assert(outputs.contains(actorAttrOutput))
  }

}
