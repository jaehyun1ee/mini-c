package miniC

import scala.util.parsing.combinator._

// Parser of miniC
trait miniCParser extends miniCAST with miniCError with RegexParsers with PackratParsers {
    object ProgramParser extends ParserObject(prog)
    object ScalarExprParser extends ParserObject(scalarExpr)
    object BoolExprParser extends ParserObject(boolExpr)
    object CmdParser extends ParserObject(cmd)
    
    abstract class ParserObject[T](parser: PackratParser[T]) {
        def apply(code: String): T = parseAll(parser, code).getOrElse(parseError(s"parsing error: $code"))
    }
    def wrapCurly[T](rule: PackratParser[T]): PackratParser[T] = "{" ~> rule <~ "}"
    def wrapParen[T](rule: PackratParser[T]): PackratParser[T] = "(" ~> rule <~ ")"
    lazy val int: PackratParser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: PackratParser[String] = regex("""[a-zA-Z][a-zA-Z0-9_-]*""".r)
    lazy val prog: PackratParser[Program] = cmd <~ opt(";") ^^ { case c => Program(c) }
    lazy val scalarExpr: PackratParser[ScalarExpr] = (
        int ^^ { case n => Num(n) } |||
        (scalarExpr <~ "+") ~ scalarExpr ^^ { case left ~ right => Add(left, right) } |||
        (scalarExpr <~ "-") ~ scalarExpr ^^ { case left ~ right => Sub(left, right) } |||
        str ^^ { case x => Id(x) }
    )
    lazy val boolExpr: PackratParser[BoolExpr] = (
        "true" ^^ { case _ => Bool(true) } |||
        "false" ^^ { case _ => Bool(false) } |||
        (str <~ "==") ~ int ^^ { case left ~ right => Eq(Id(left), Num(right)) } |||
        (str <~ "<") ~ int ^^ { case left ~ right => Lt(Id(left), Num(right)) }
    )
    lazy val cmd: PackratParser[Cmd] = (
        "Skip" ^^ { case _ => Skip() } |||
        (cmd <~ ";") ~ cmd ^^ { case head ~ tail => Seq(head, tail) } |||
        (str <~ ":=") ~ scalarExpr ^^ { case x ~ e => Assign(x, e) } |||
        ("input(" ~> str) <~ ")" ^^ { case x => In(x) } |||
        ("if" ~> wrapParen(boolExpr)) ~ wrapCurly(cmd) ~ ("else" ~> wrapCurly(cmd)) ^^ { case cond ~ trueBranch ~ falseBranch => Branch(cond, trueBranch, falseBranch) } |||
        ("while" ~> wrapParen(boolExpr)) ~ wrapCurly(cmd) ^^ { case cond ~ body => While(cond, body) }
    )
}