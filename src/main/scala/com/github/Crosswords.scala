package com.github

import scala.collection.mutable

case class Crosswords(width: Int, height: Int, placeholders: List[Placeholder]) {

  private val intersections = new mutable.ListBuffer[Intersection]

  def addIntersection(intersection: Intersection): Unit = {
    intersections += intersection
    intersections += Intersection( intersection.to, intersection.from )
  }

  def getIntersectionBetween(placeholderId1: String, placeholderId2: String): Option[Intersection] = {
    intersections.find( x => x.from.placeholderId.equals(placeholderId1) &&
                             x.to.placeholderId.equals(placeholderId2))
  }

  def getIntersectionsFor(placeholderId: String): List[Intersection] = {
    intersections.filter( x => x.from.placeholderId == placeholderId ).toList
  }

  def getPlaceholderById(id: String): Placeholder = placeholders.find( x => x.id.equals(id) ).get

  private lazy val pruneCandidateValues: List[Placeholder] = {
    placeholders.map( placeholder => pruneCandidateValues(placeholder, getIntersectionsFor(placeholder.id)) )
  }

  private def pruneCandidateValues(placeholder: Placeholder, intersections: List[Intersection]): Placeholder = {
    var thisCandidateValues = placeholder.candidateValues

    intersections.foreach { intersection =>
      val otherCandidateValues = getPlaceholderById(intersection.to.placeholderId).candidateValues

      val newSet = for {
        w1 <- thisCandidateValues
        w2 <- otherCandidateValues
        newW = if (w1(intersection.from.index) == w2(intersection.to.index)) Some(w1) else None
      } yield newW

      thisCandidateValues = newSet.filter {
        case None => false
        case Some(_) => true
      } map (someString => someString.get)
    }

    placeholder.copy(candidateValues = thisCandidateValues)
  }

  lazy val solveAllPossibilities: List[Crosswords] = {

    val all = pruneCandidateValues.map(_.allPossibilities)
    val result = new mutable.ListBuffer[Crosswords]

    def move(i: Int, list: mutable.ListBuffer[Placeholder]): List[Placeholder] = {
      if (i >= all.size)
        list.toList
      else {
        for (j <- all(i).indices) {
          list += all(i)(j)
          val phs = move(i+1, list)
          if (i == all.size-1 && phs.size == all.size && isValidSolution(phs)){
            result += copy(placeholders = phs)
          }
          list -= all(i)(j)
        }
        list.toList
      }
    }

    move(0, new mutable.ListBuffer[Placeholder])

    result.toList
  }

  def isValidSolution(placeholders: List[Placeholder]): Boolean ={
    val list = placeholders.zipWithIndex.map( x => (x._1 , placeholders.drop(x._2+1)) )
    val booleans = list.flatMap(x => x._2.map(pl => isSatisfyIntersection(x._1, pl)))

    booleans.forall(_ == true)
  }

  def isSatisfyIntersection(placeholder1: Placeholder, placeholder2: Placeholder): Boolean = {
    getIntersectionBetween( placeholder1.id , placeholder2.id ) match {
      case None => true
      case Some(intersection) =>
        placeholder1.currentValue.get.charAt(intersection.from.index) == placeholder2.currentValue.get.charAt(intersection.to.index)
    }
  }

  def render(): Unit = {

    val table = Array.fill(width,height)('+')

    placeholders.foreach { placeholder =>
      val pos = placeholder.position
      val value = placeholder.currentValue

      def getValue(index: Int): Char = value match {
        case None => '-'
        case Some(input) => input.charAt(index)
      }

      placeholder.direction match {
        case Direction.Horizontal =>
          for (col <- pos.col until pos.col+placeholder.size) {
            table(pos.row)(col) = getValue(col - pos.col)
          }
        case Direction.Vertical =>
          for (row <- pos.row until pos.row+placeholder.size) {
            table(row)(pos.col) = getValue(row - pos.row)
          }
      }
    }

    table.foreach{ row =>
      row.foreach(print(_))
      println()
    }
  }

}
