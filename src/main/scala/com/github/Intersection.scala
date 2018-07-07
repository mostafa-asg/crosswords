package com.github

case class PlaceholderCell(placeholderId: String, index: Int)

/**
  * Holds information of intersection between two placeholder
  * @param from
  * @param to
  */
case class Intersection(from: PlaceholderCell, to: PlaceholderCell)

object Intersection {
  def apply(from: (String,Int), to: (String,Int)): Intersection = {
    new Intersection(PlaceholderCell(from._1,from._2), PlaceholderCell(to._1,to._2))
  }
}