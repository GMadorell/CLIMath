import $ivy.`com.lihaoyi::fastparse:2.2.2`, fastparse._
import SingleLineWhitespace._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

val help =
  """CLIMath - No Non-sense Arithmetic Operations on the CLI
Usage:

$ amm --silent CliMath.sc 10 + 4 + 2
> 16

$ amm --silent CliMath.sc "10 * (2 + 1.24)"
> 30.24

$ amm --silent CliMath.sc 100 000. 01 + 345,545.12 + 456.678.213,39
> 457123758.52
"""

@main
def main(expressions: String*): Unit = expressions.toList match {
  case Nil | "-h" :: _ | "-help" :: _ | "help" :: _ | "--h" :: _ |
      "--help" :: _ =>
    println(help)

  case other =>
    calculate(other.mkString(" ")) match {
      case failure: Failure =>
        println("Couldn't parse expression")
        println(failure.trace().failure.label)
      case Success(value, _) =>
        println(value.bigDecimal.toPlainString())
    }
}

def unsignedInteger[_: P]: P[String] =
  P((" ".rep(0) ~ CharIn("0-9").! ~~ (" ".rep(0) ~ CharIn("0-9").!).rep(0)))
    .map { case (first, others) =>
      first + others.mkString
    }

def signedInteger[_: P]: P[String] =
  ("-" ~ unsignedInteger).map(unsigned => s"-$unsigned")

def anyInteger[_: P]: P[String] =
  P(unsignedInteger | signedInteger)

def singleCommaDecimal[_: P]: P[String] =
  P(anyInteger ~ "," ~ unsignedInteger).map { case (nonDecimal, decimal) =>
    s"$nonDecimal.$decimal"
  }

def singleDottedDecimal[_: P]: P[String] =
  P(anyInteger ~ "." ~ unsignedInteger).map { case (nonDecimal, decimal) =>
    s"$nonDecimal.$decimal"
  }

def unsignedIntegerLimitedAtThree[_: P]: P[String] =
  CharIn("0-9").rep(min = 1, max = 3).!

// Numbers shaped like: -120,000.99
def usNotationDecimal[_: P]: P[String] =
  P(
    "-".!.? ~
      unsignedIntegerLimitedAtThree ~
      ("," ~ unsignedIntegerLimitedAtThree).rep(1) ~
      "." ~ unsignedInteger
  ).map { case (maybeSign, firstNonDecimal, otherNonDecimal, decimal) =>
    s"${maybeSign.getOrElse("")}$firstNonDecimal${otherNonDecimal.mkString}.$decimal"
  }

// Numbers shaped like: -120.000,99
def euNotationDecimal[_: P]: P[String] =
  P(
    "-".!.? ~
      unsignedIntegerLimitedAtThree ~
      ("." ~ unsignedIntegerLimitedAtThree).rep(1) ~
      "," ~ unsignedInteger
  ).map { case (maybeSign, firstNonDecimal, otherNonDecimal, decimal) =>
    s"${maybeSign.getOrElse("")}$firstNonDecimal${otherNonDecimal.mkString}.$decimal"
  }

def number[_: P]: P[BigDecimal] = P(
  euNotationDecimal |
    usNotationDecimal |
    singleDottedDecimal |
    singleCommaDecimal |
    anyInteger
).map(BigDecimal.exact)

def operate(operations: (BigDecimal, Seq[(String, BigDecimal)])) =
  operations match {
    case (firstNumber, otherOperations) =>
      otherOperations.foldLeft(firstNumber) {
        case (accumulator, ("+", number)) => accumulator + number
        case (accumulator, ("-", number)) => accumulator - number
        case (accumulator, ("*", number)) => accumulator * number
        case (accumulator, ("/", number)) => accumulator / number
      }
  }

def factor[_: P]: P[BigDecimal] =
  P(number | ("(" ~ addAndSub ~ ")"))

def mulAndDiv[_: P]: P[BigDecimal] =
  P(factor ~ (("*" | "/").! ~ factor).rep).map(operate)

def addAndSub[_: P]: P[BigDecimal] =
  P(mulAndDiv ~ (("+" | "-").! ~ mulAndDiv).rep).map(operate)

def parser[_: P]: P[BigDecimal] =
  P(" ".rep ~ addAndSub ~ " ".rep ~ End)

def calculate(mathExpression: String): Parsed[BigDecimal] =
  parse(mathExpression, parser(_))
