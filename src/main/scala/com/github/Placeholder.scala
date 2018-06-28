package com.github

import com.github.Direction.Direction

case class Placeholder(id: String,
                       size: Int,
                       direction: Direction,
                       candidateValues: Set[String],
                       currentValue: Option[String] = None,
                       position: Position = Position(0,0)
                      ) {

  lazy val allPossibilities: List[Placeholder] = {
    candidateValues.toList.map(candidate => copy(currentValue = Some(candidate)))
  }

}
