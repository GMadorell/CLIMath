import $ivy.`com.lihaoyi::utest:0.7.2`
import $ivy.`com.lihaoyi::fastparse:2.2.2`

import utest._
import fastparse.Parsed
import $file.CLIMath

val tests = utest.Tests {
  "Handle single unsigned non-decimal numbers" - {
    assertSuccess(CLIMath.calculate("0"), 0.0d)
    assertSuccess(CLIMath.calculate("1"), 1.0d)
    assertSuccess(CLIMath.calculate("94"), 94.0d)
    assertSuccess(CLIMath.calculate("124"), 124.0d)
  }
  "Handle single negative non-decimal numbers" - {
    assertSuccess(CLIMath.calculate("-0"), 0.0d)
    assertSuccess(CLIMath.calculate("-1"), -1.0d)
    assertSuccess(CLIMath.calculate("-94"), -94.0d)
    assertSuccess(CLIMath.calculate("-124"), -124.0d)
  }
  "Handle single decimal numbers assuming SI notation" - {
    assertSuccess(CLIMath.calculate("0,1"), 0.1d)
    assertSuccess(CLIMath.calculate("0,01"), 0.01d)
    assertSuccess(CLIMath.calculate("1234,123"), 1234.123d)
    assertSuccess(CLIMath.calculate("-1234,123"), -1234.123d)
    assertSuccess(CLIMath.calculate("0.1"), 0.1d)
    assertSuccess(CLIMath.calculate("0.01"), 0.01d)
    assertSuccess(CLIMath.calculate("1234.123"), 1234.123d)
    assertSuccess(CLIMath.calculate("-1234.123"), -1234.123d)
  }
  "Handle single decimal numbers assuming UK/US/Australia notation" - {
    assertSuccess(CLIMath.calculate("120,000.99"), 120000.99d)
    assertSuccess(CLIMath.calculate("-120,000.99"), -120000.99d)
  }
  "Handle single decimal numbers assuming EU notation" - {
    assertSuccess(CLIMath.calculate("120.000,99"), 120000.99d)
    assertSuccess(CLIMath.calculate("-120.000,99"), -120000.99d)
  }
  "Sum numbers" - {
    assertSuccess(CLIMath.calculate("1+2"), 3.0d)
    assertSuccess(CLIMath.calculate("1+2+3"), 6.0d)
    assertSuccess(CLIMath.calculate("1+2+3+4+5"), 15.0d)
    assertSuccess(CLIMath.calculate("120+240"), 360.0d)
    assertSuccess(CLIMath.calculate("120+240+10010"), 10370.0d)
    assertSuccess(CLIMath.calculate("120 + 240"), 360.0d)
    assertSuccess(CLIMath.calculate("1.123+123.13"), 124.253d)
  }
  "Subtract numbers" - {
    assertSuccess(CLIMath.calculate("1-2"), -1.0d)
    assertSuccess(CLIMath.calculate("1-2-3"), -4.0d)
    assertSuccess(CLIMath.calculate("130-25,5"), 104.5d)
  }

  def assertSuccess(result: Parsed[Double], expected: Double) = {
    assert(result match {
      case Parsed.Success(actualResult, _) => actualResult == expected
      case _                               => false
    })
  }
}

TestRunner.runAndPrint(tests, "CLIMathSpec")
