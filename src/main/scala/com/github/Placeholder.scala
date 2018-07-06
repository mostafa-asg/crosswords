package com.github

import com.github.Direction.Direction

/**
  * Placeholder represents a place that can fill with letters
  * @param id each placeholder has a id, id is used for defining intersections between placeholders
  * @param size the place holder size, or how many character can be in this placeholder
  * @param direction eighter Vertical or Horizontal
  * @param startPosition a point position contains x and y for rendering this placeholder withing crosswords
  * @param candidateValues all possible candidate words that can replace to this placeholder
  * @param currentValue current fill word
  */
case class Placeholder(id: String,
                       size: Int,
                       direction: Direction,
                       startPosition: Point = Point(0,0),
                       candidateValues: Set[String] = Set.empty,
                       currentValue: Option[String] = None,
                      ) {

  require(id.nonEmpty, "You must provide an identifier for this placeholder")
  require(size > 0, "Size must be positive")
  require(candidateValues.forall(_.length == size), s"these words are not perfect candidate ${candidateValues.filter(_.length != size)}")

  /**
    * Returns all the possibilities of this placeholder based on candidateValues
    * for example if candidateValues are MOSTAFA and IRELAND it returns
    * List(
    *   Placeholder( ... , value = MOSTAFA)
    *   Placeholder( ... , value = IRELAND)
    * )
    */
  lazy val allPossibilities: List[Placeholder] = {
    candidateValues.toList.map(candidate => copy(currentValue = Some(candidate)))
  }

  /**
    * Returns all the position points based on startPosition, size and direction
    * Example:
    *   startPosition = Point( x = 2, y = 5)
    *   size = 3
    *   direction = Vertical
    * Returns:
    *   List(
    *     Point(2,5),
    *     Point(3,5),
    *     Point(4,5)
    *   )
    */
  lazy val getPositionPoints: List[Point] = {
    val positions = List.fill[Point](size)(startPosition)

    direction match {
      case Direction.Horizontal =>
        positions.zipWithIndex.map { x =>
          val point = x._1
          point.copy( col = point.col + x._2 )
        }
      case Direction.Vertical =>
        positions.zipWithIndex.map { x =>
          val point = x._1
          point.copy( row = point.row + x._2 )
        }
    }
  }

  /**
    * Returns a new placeholder with given candidate values
    * @param values a set of words, it will ignore all words that cannot fit to this placeholder
    * @return new placeholder
    */
  def withCandidateValues(values: Set[String]): Placeholder = {
    copy( candidateValues = values.filter( _.length == size ) )
  }

  /**
    * Give a Point and returns the word index
    * Example:
    *   size : 4
    *   startPosition: Point(3,5)
    *   direction: Vertical
    *   maybePoint: Point(4,5)
    * Returns:
    *   1 because it is the second letter
    */
  def pointToIndex(maybePoint: Option[Point]): Option[Int] = maybePoint match {
    case Some(point) => pointToIndex(point)
    case _ => None
  }

  /**
    * Give a Point and returns the word index
    * Example:
    *   size : 4
    *   startPosition: Point(3,5)
    *   direction: Vertical
    *   maybePoint: Point(4,5)
    * Returns:
    *   1 because it is the second letter
    */
  def pointToIndex(point: Point): Option[Int] = getPositionPoints.zipWithIndex.find(x => x._1.equals(point)).flatMap(tuple => Some(tuple._2))

}

object Placeholder {

  /**
    * Returns a horizontal placeholder
    */
  def horizontal(id: String,
                 size: Int,
                 startPosition: Point = Point(0,0),
                 candidateValues: Set[String] = Set.empty,
                 currentValue: Option[String] = None): Placeholder = {
    Placeholder(id, size, Direction.Horizontal, startPosition, candidateValues, currentValue)
  }

  /**
    * Returns a vertical placeholder
    */
  def vertical(id: String,
               size: Int,
               startPosition: Point = Point(0,0),
               candidateValues: Set[String] = Set.empty,
               currentValue: Option[String] = None): Placeholder = {
    Placeholder(id, size, Direction.Vertical, startPosition, candidateValues, currentValue)
  }

}