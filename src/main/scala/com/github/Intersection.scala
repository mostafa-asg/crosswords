package com.github

case class PlaceholderCell(placeholderId: String, index: Int)

case class Intersection(from: PlaceholderCell, to: PlaceholderCell)
