package com.github

import org.scalatest.FlatSpec

class StringUtilSpec extends FlatSpec {

  "times" should "concat the string n times" in {
    import StringUtil._

    assert("ABC-".times(3).equals("ABC-ABC-ABC-"))
  }

}
