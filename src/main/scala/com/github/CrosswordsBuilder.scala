package com.github

import com.github.Direction.Direction
import scala.collection.mutable

object CrosswordsBuilder {

  def buildFrom(crosswordsInString: String, words: Set[String]): Crosswords = {

    val rows = crosswordsInString.split("\n")
    val cols = rows(0).length
    val verticalPlaceholders = new mutable.ListBuffer[Placeholder]
    val horizontalPlaceholders = new mutable.ListBuffer[Placeholder]

    def swapRowsWithColumns: Array[String] = {
      val tempArr = Array.ofDim[Char](rows.length, cols)
      val strBuilder = new mutable.StringBuilder
      val result = Array.ofDim[String](cols)

      for (row <- 0 until rows.length) {
        for (col <- 0 until cols) {
          tempArr(row)(col) = rows(row).charAt(col)
        }
      }

      for (col <- 0 until cols) {
        for (row <- 0 until rows.length) {
          strBuilder += tempArr(row)(col)
        }
        result(col) = strBuilder.toString()
        strBuilder.clear()
      }

      result
    }

    rows.zipWithIndex.foreach { row =>
      val rowIndex = row._2
      val content = row._1
      horizontalPlaceholders ++= findAllPlaceholders(rowIndex, content, Direction.Horizontal)
    }

    swapRowsWithColumns.zipWithIndex.foreach { col =>
      val colIndex = col._2
      val content = col._1
      verticalPlaceholders ++= findAllPlaceholders(colIndex, content, direction = Direction.Vertical)
    }

    val intersections = findIntersectionsBetween(horizontalPlaceholders.toList, verticalPlaceholders.toList)
    val placeholders = (horizontalPlaceholders ++ verticalPlaceholders).toList.map(_.withCandidateValues(words))

    val crosswords = Crosswords(rows.length, cols, placeholders)
    intersections.foreach(crosswords.addIntersection)

    crosswords
  }

  private def findIntersectionsBetween(horizontalPlaceholders: List[Placeholder],
                               verticalPlaceholders: List[Placeholder]): List[Intersection] = {

    def getIntersection(ph1: Placeholder,
                        ph2: Placeholder,
                        indexes: (Option[Int],Option[Int])): Option[Intersection] = indexes match {
      case (Some(index1),Some(index2)) => Some(Intersection(PlaceholderCell(ph1.id,index1), PlaceholderCell(ph2.id, index2)))
      case _ => None
    }

    val intersections = for {
      ph1 <- horizontalPlaceholders
      ph2 <- verticalPlaceholders
      intersectPoint = (ph1.getPositionPoints intersect ph2.getPositionPoints).headOption
      indexes = (ph1.pointToIndex(intersectPoint), ph2.pointToIndex(intersectPoint))
      intersection = getIntersection(ph1, ph2, indexes)
    } yield intersection

    intersections.filter {
      case Some(_) => true
      case None => false
    } map {
      someIntersection => someIntersection.get
    }
  }

  private def findAllPlaceholders(number: Int, data: String, direction: Direction): List[Placeholder] = {
    val placeholders = new mutable.ListBuffer[Placeholder]
    var size = 0
    var index = 0
    var foundPlaceholder = false

    data.zipWithIndex.foreach { ch =>
      if (ch._1 == '-' && !foundPlaceholder){
        index = ch._2
        foundPlaceholder = true
      } else if (ch._1 == '+' && foundPlaceholder) {
        size = ch._2 - index
        foundPlaceholder = false
        if (size > 1) {
          val (id, position) = direction match {
            case Direction.Vertical => (s"C$index-$number", Point(index, number)) // number is column number , index is row number
            case Direction.Horizontal => (s"R$number-$index", Point(number, index)) //number is row number, index is column number
          }
          placeholders += Placeholder(id, size, direction, startPosition = position)
        }
      }
    }

    placeholders.toList
  }

}
