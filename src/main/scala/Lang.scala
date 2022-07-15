/*
package miniC

import scala.util.parsing.combinator._

trait miniC extends LangError with RegexParsers with PackratParsers {
    // Mini C Program
    case class Program(cmd: Cmd)
    object Program extends ParserObject(prog)

    // Scalar Expressions
    trait ScalarExpr // E ::=
    case class Num(num: Int) extends ScalarExpr   // n
    case class Add(left: ScalarExpr, right: ScalarExpr) extends ScalarExpr // e1 + e2
    case class Sub(left: ScalarExpr, right: ScalarExpr) extends ScalarExpr // e1 - e2
    case class Id(name: String) extends ScalarExpr // x
    object ScalarExpr extends ParserObject(scalarExpr)

    // Boolean Expressions
    trait BoolExpr // B ::=
    case class Bool(bool: Boolean) extends BoolExpr // b
    case class Eq(left: String, right: Num) extends BoolExpr // x == n
    case class Lt(left: String, right: Num) extends BoolExpr // x > n
    object BoolExpr extends ParserObject(boolExpr)

    // Commands
    trait Cmd // C ::=
    case class Skip() extends Cmd // skip;
    case class Seq(head: Cmd, tail: Cmd) extends Cmd // C; C
    case class Assign(name: String, expr: ScalarExpr) extends Cmd // x := E
    case class In(name: String) extends Cmd // input(x)
    case class Branch(cond: BoolExpr, trueBranch: Cmd, falseBranch: Cmd) extends Cmd // if(be){c}else{c}
    case class While(cond: BoolExpr, body: Cmd) extends Cmd // while(b){c}
    object Cmd extends ParserObject(cmd)

    // Environment
    type Env = Map[String, Int]

    // Runner
    def run(code: String): String = {
        val p: Program = Program(code)
        p.toString
        //interp(p).toString
    }

    // Parser
    abstract class ParserObject[T](parser: PackratParser[T]) {
        def apply(code: String): T = parseAll(parser, code).getOrElse(parseError(s"bad syntax: $code"))
    }
    def wrap[T](rule: PackratParser[T]): PackratParser[T] = "{" ~> rule <~ "}"
    lazy val int: PackratParser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: PackratParser[String] = regex("""[a-zA-Z][a-zA-Z0-9_-]*""".r)
    lazy val prog: PackratParser[Program] = cmd <~ opt(";") ^^ { case c => Program(c) }
    lazy val scalarExpr: PackratParser[ScalarExpr] = (
        int ^^ { case n => Num(n) } |||
        (scalarExpr <~ "+") ~ scalarExpr ^^ { case se1 ~ se2 => Add(se1, se2) } |||
        (scalarExpr <~ "-") ~ scalarExpr ^^ { case se1 ~ se2 => Sub(se1, se2) } |||
        str ^^ { case x => Id(x) }
    )
    lazy val boolExpr: PackratParser[BoolExpr] = (
        "true" ^^ { case _ => Bool(true) } |||
        "false" ^^ { case _ => Bool(false) } |||
        (str <~ "==") ~ int ^^ { case x ~ n => Eq(x, Num(n)) } |||
        (str <~ ">") ~ int ^^ { case x ~ n => Lt(x, Num(n)) }
    )
    lazy val cmd: PackratParser[Cmd] = (
        "Skip" ^^ { case _ => Skip() } |||
        (cmd <~ ";") ~ cmd ^^ { case c1 ~ c2 => Seq(c1, c2) } |||
        (str <~ ":=") ~ scalarExpr ^^ { case x ~ se => Assign(x, se) } |||
        ("input(" ~> str) <~ ")" ^^ { case x => In(x) } |||
        (("if(" ~> boolExpr) <~ ")") ~ (wrap(cmd)) ~ ("else" ~> wrap(cmd)) ^^ { case be ~ c1 ~ c2 => Branch(be, c1, c2) } |||
        (("while(" ~> boolExpr) <~ ")") ~ (wrap(cmd)) ^^ { case be ~ c => While(be, c) }
    )

    // Interpreter
    /*
    def interp(expr: ScalarExpr, env: Env): Int
    def interp(expr: BoolExpr, env: Env): Boolean
    def interp(cmd: Cmd, env: Env): Env
    def interp(prog: Program): Env
    */
}
*/