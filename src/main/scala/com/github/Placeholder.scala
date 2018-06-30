package com.github

import com.github.Direction.Direction

case class Placeholder(id: String,
                       size: Int,
                       direction: Direction,
                       startPosition: Point = Point(0,0),
                       candidateValues: Set[String] = Set.empty,
                       currentValue: Option[String] = None,
                      ) {

  require(candidateValues.forall(_.length == size), s"these words are not perfect candidate ${candidateValues.filter(_.length != size)}")

  lazy val allPossibilities: List[Placeholder] = {
    candidateValues.toList.map(candidate => copy(currentValue = Some(candidate)))
  }

  val getPositionPoints: List[Point] = {
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

  def withCandidateValues(values: Set[String]): Placeholder = {
    copy( candidateValues = values.filter( _.length == size ) )
  }

  def pointToIndex(maybePoint: Option[Point]): Option[Int] = maybePoint match {
    case Some(point) => pointToIndex(point)
    case _ => None
  }

  def pointToIndex(point: Point): Option[Int] = getPositionPoints.zipWithIndex.find(x => x._1.equals(point)).flatMap(tuple => Some(tuple._2))

}