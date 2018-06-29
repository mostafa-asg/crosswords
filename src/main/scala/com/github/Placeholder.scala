package com.github

import com.github.Direction.Direction

case class Placeholder(id: String,
                       size: Int,
                       direction: Direction,
                       candidateValues: Set[String],
                       currentValue: Option[String] = None,
                       startPosition: Point = Point(0,0)
                      ) {

  lazy val allPossibilities: List[Placeholder] = {
    candidateValues.toList.map(candidate => copy(currentValue = Some(candidate)))
  }

  val getPositionPoints: Set[Point] = {
    val positions = List.fill[Point](size)(startPosition)

    direction match {
      case Direction.Horizontal =>
        positions.zipWithIndex.map { x =>
          val point = x._1
          point.copy( col = point.col + x._2 )
        }.toSet
      case Direction.Vertical =>
        positions.zipWithIndex.map { x =>
          val point = x._1
          point.copy( row = point.row + x._2 )
        }.toSet
    }
  }

  def pointToIndex(point: Point): Int = {
    getPositionPoints.zipWithIndex.find(x => x._1.equals(point)).map( _._2 ).getOrElse(-1)
  }

}