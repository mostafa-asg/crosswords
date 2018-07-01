package com.github

import org.scalatest.{FlatSpec, Matchers}

class PlaceholderSpec extends FlatSpec with Matchers {

  "Empty id" should "throws exception" in {
    assertThrows[IllegalArgumentException](Placeholder.horizontal("", 15))
  }

  "Invalid size" should "throws exception" in {
    assertThrows[IllegalArgumentException](Placeholder.vertical("someId", -6))
  }

  "Candidate values with invalid size" should "throws exception" in {
    assertThrows[IllegalArgumentException](Placeholder("someId", 5, Direction.Vertical, candidateValues = Set("a")))
  }

  "All possibilities" should "return valid list" in {
    val placeholder = Placeholder("someId", 3, Direction.Horizontal, candidateValues = Set("ABC","XYZ","air"))
    val allPossibilities = placeholder.allPossibilities

    val expectedList = List(
      placeholder.copy(currentValue = Some("ABC")),
      placeholder.copy(currentValue = Some("XYZ")),
      placeholder.copy(currentValue = Some("air"))
    )

    assert( allPossibilities.size == 3 )
    expectedList.foreach(item => assert(allPossibilities.contains(item)))
  }

  "Position points" should "be valid" in {
    val horizontalPlaceholder = Placeholder.horizontal("someId", 5, startPosition = Point(4,3))
    var expectedPoints = List(Point(4,3), Point(4,4), Point(4,5), Point(4,6), Point(4,7))
    assert(horizontalPlaceholder.getPositionPoints.equals(expectedPoints))

    val verticalPlaceholder = Placeholder.vertical("someId", 4, startPosition = Point(1,2))
    expectedPoints = List(Point(1,2), Point(2,2), Point(3,2), Point(4,2))
    assert(verticalPlaceholder.getPositionPoints.equals(expectedPoints))
  }

  "Point in placeholder boundary" should "have corresponding index" in {
    val horizontalPlaceholder = Placeholder.horizontal("someId", 3, startPosition = Point(1,1))
    assert( horizontalPlaceholder.pointToIndex(Point(1,3)).get == 2)
    assert( horizontalPlaceholder.pointToIndex(Some(Point(1,1))).get == 0)

    val verticalPlaceholder = Placeholder.vertical("someId", 4, startPosition = Point(3,1))
    assert( verticalPlaceholder.pointToIndex(Point(4,1)).get == 1)
    assert( verticalPlaceholder.pointToIndex(Some(Point(6,1))).get == 3)
  }

  "Point outside a placeholder" should "return None" in {
    val horizontalPlaceholder = Placeholder.horizontal("someId", 3, startPosition = Point(1,1))
    val verticalPlaceholder = Placeholder.vertical("someId", 4, startPosition = Point(3,1))

    assert( horizontalPlaceholder.pointToIndex(Point(1,4)).isEmpty )
    assert( horizontalPlaceholder.pointToIndex(Some(Point(0,1))).isEmpty )
    assert( horizontalPlaceholder.pointToIndex(None).isEmpty )

    assert( verticalPlaceholder.pointToIndex(Point(2,1)).isEmpty )
    assert( verticalPlaceholder.pointToIndex(Some(Point(7,1))).isEmpty )
    assert( verticalPlaceholder.pointToIndex(None).isEmpty )
  }
}