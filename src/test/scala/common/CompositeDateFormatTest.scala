package common

import org.scalatest.FreeSpec

class CompositeDateFormatTest extends FreeSpec with AuxSpecs {
  "Parsing should" - {
    ",when using a single formatter," - {
      val $ = CompositeDateFormat("yyyy-MM-dd")
      "take the first match" in {
        val result = $.parse("1985-08-28")
        result.getYear shouldReturn 1985
        result.getMonthOfYear shouldReturn 8
        result.getDayOfMonth shouldReturn 28
      }
      "Throw an exception if no match has been found" in { an[Exception] should be thrownBy { $.parse("19850828") } }
      "Print the correct date" in { $.print(0) shouldReturn "1970-01-01" }
      "Parse using a whitespace formatter" in {
        val result = CompositeDateFormat("yyyy-MM-dd HH:mm:ss").parse("2011-04-01 02:33:31")
        result.getYear shouldReturn 2011
        result.getMonthOfYear shouldReturn 4
        result.getDayOfMonth shouldReturn 1
        result.getHourOfDay shouldReturn 2
        result.getMinuteOfHour shouldReturn 33
        result.getSecondOfMinute shouldReturn 31
      }
    }
    ",when using more than a single formatter," - {
      val $ = CompositeDateFormat(
        "yyyy-MM-dd",
        "dd-MM-yyyy",
        "yyyy-MM"
      )
      "take the first match" in {
        val result = $.parse("1985-08-28")
        result.getYear shouldReturn 1985
        result.getMonthOfYear shouldReturn 8
        result.getDayOfMonth shouldReturn 28
      }
      "Use the second formatter if the first fails" - {
        val result = $.parse("28-08-1985")
        result.getYear shouldReturn 1985
        result.getMonthOfYear shouldReturn 8
        result.getDayOfMonth shouldReturn 28
      }
      "Throw an exception if no match has been found" in { an[Exception] should be thrownBy { $.parse("19850828") } }
      "Print the correct date according to the first formatter" in { $.print(0) shouldReturn "1970-01-01" }
      "Take the best match, and not parse greedily" in {
        val $ = CompositeDateFormat(
          "yyyy-MM",
          "yyyy-MM-dd"
        )
        $.parse("1985-08-28").getDayOfMonth shouldReturn 28
      }
    }
  }
}
