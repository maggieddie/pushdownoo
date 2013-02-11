package org.ucombinator.testparsing
import scala.util.parsing.combinator.RegexParsers


  
  object Calculator extends RegexParsers {
     def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
     def factor: Parser[Double] = number | "(" ~> expr <~ ")"
     def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
       case number ~ list => (number /: list) {
         case (x, "*" ~ y) => x * y
         case (x, "/" ~ y) => x / y
       }
     }
     def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
       case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
         case (x, "+" ~ y) => x + y
         case (x, "-" ~ y) => x - y
       }
     }
 
    def apply(input: String): Double = parseAll(expr, input) match {
       case Success(result, _) => result
       case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }

