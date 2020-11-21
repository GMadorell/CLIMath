import $ivy.`com.lihaoyi::fastparse:2.2.2`, fastparse._, SingleLineWhitespace._

@main
def main(expressions: String*): Unit = {
  println(expressions)
}

def unsignedInteger[_: P] =
  CharIn("0-9").rep(1).!

def signedInteger[_: P] =
  ("-" ~ unsignedInteger).map(unsigned => s"-$unsigned")

def anyInteger[_: P] =
  P(unsignedInteger | signedInteger)

def singleCommaDecimal[_: P] =
  P(anyInteger ~ "," ~ unsignedInteger).map { case (nonDecimal, decimal) =>
    s"$nonDecimal.$decimal"
  }

def singleDottedDecimal[_: P] =
  P(anyInteger ~ "." ~ unsignedInteger).map { case (nonDecimal, decimal) =>
    s"$nonDecimal.$decimal"
  }

def unsignedIntegerLimitedAtThree[_: P] =
  CharIn("0-9").rep(min = 1, max = 3).!

// Numbers shaped like: -120,000.99
def usNotationDecimal[_: P] =
  P(
    "-".!.? ~
      unsignedIntegerLimitedAtThree ~
      ("," ~ unsignedIntegerLimitedAtThree).rep(1) ~
      "." ~ unsignedInteger
  ).map { case (maybeSign, firstNonDecimal, otherNonDecimal, decimal) =>
    s"${maybeSign.getOrElse("")}$firstNonDecimal${otherNonDecimal.mkString}.$decimal"
  }

// Numbers shaped like: -120.000,99
def euNotationDecimal[_: P] =
  P(
    "-".!.? ~
      unsignedIntegerLimitedAtThree ~
      ("." ~ unsignedIntegerLimitedAtThree).rep(1) ~
      "," ~ unsignedInteger
  ).map { case (maybeSign, firstNonDecimal, otherNonDecimal, decimal) =>
    s"${maybeSign.getOrElse("")}$firstNonDecimal${otherNonDecimal.mkString}.$decimal"
  }

def number[_: P] = P(
  euNotationDecimal |
    usNotationDecimal |
    singleDottedDecimal |
    singleCommaDecimal |
    anyInteger
).map(_.toDouble)

def operator[_: P] = P("+" | "-").!

def expression[_: P] = P(
  number ~ (operator ~ number).rep(0)
)

def parser[_: P]: P[Double] = P(expression.map {
  case (head, tail) => tail.foldLeft(head) {
    case (accumulator, ("+", number)) => accumulator + number
    case (accumulator, ("-", number)) => accumulator - number
  }
} ~ End)

def calculate(mathExpression: String): Parsed[Double] =
  parse(mathExpression, parser(_))