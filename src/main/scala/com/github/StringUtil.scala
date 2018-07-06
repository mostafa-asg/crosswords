package com.github

object StringUtil {

  implicit class StringOps(input: String) {

    /**
      * Concat the input to itself `number` of times
      * @param number number of times to concat
      */
    def times(number: Int): String = {
      val builder = new StringBuilder
      (1 to number).foreach( _ => builder.append(input) )
      builder.toString()
    }

  }

}
