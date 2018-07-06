package com.github

case class PlaceholderCell(placeholderId: String, index: Int)

/**
  * Holds information of intersection between two placeholder
  * @param from
  * @param to
  */
case class Intersection(from: PlaceholderCell, to: PlaceholderCell)
