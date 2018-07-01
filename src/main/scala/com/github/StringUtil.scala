package com.github

object StringUtil {

  implicit class StringOps(input: String) {

    def times(number: Int): String = {
      val builder = new StringBuilder
      (1 to number).foreach( _ => builder.append(input) )
      builder.toString()
    }

  }

}
