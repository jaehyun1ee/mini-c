package miniC

// AST of miniC
trait miniCAST {
    // Pretty printer
    def pretty(scalarExpr: ScalarExpr): String = scalarExpr match {
        case Num(n) => n.toString
        case Add(left, right) => pretty(left) + " + " + pretty(right)
        case Sub(left, right) => pretty(left) + " - " + pretty(right)
        case Id(x) => x
    }
    def pretty(boolExpr: BoolExpr): String = boolExpr match {
        case Bool(b) => b.toString
        case Eq(left, right) => pretty(left) + " == " + pretty(right)
        case Lt(left, right) => pretty(left) + " < " + pretty(right)
    }
    def pretty(cmd: Cmd, indent: String): String = cmd match {
        case Skip() => indent + "Skip"
        case Seq(head, tail) => pretty(head, indent) + ";\n" + pretty(tail, indent)
        case Assign(name, expr) => indent + name + " := " + pretty(expr)
        case In(name) => indent + "input(" + name + ")"
        case Branch(cond, trueBranch, falseBranch) => indent + "if(" + pretty(cond) + ")" + indent + "{\n" + pretty(trueBranch, indent + "   ") + "\n" + indent + "}\n" + indent + "else {\n" + pretty(falseBranch, indent + "   ") + "\n" + indent + "}"
        case While(cond, body) => indent + "while(" + pretty(cond) + ")" + indent + "{\n" + pretty(body, indent + "   ") + "\n" + indent + "}"
    }
    def pretty(prog: Program): String = {
        pretty(prog.cmd, "")
    }
}

// Mini C Program
case class Program(cmd: Cmd) extends miniCAST

// Scalar Expressions
trait ScalarExpr extends miniCAST // E ::=
case class Num(num: Int) extends ScalarExpr   // n
case class Add(left: ScalarExpr, right: ScalarExpr) extends ScalarExpr // e1 + e2
case class Sub(left: ScalarExpr, right: ScalarExpr) extends ScalarExpr // e1 - e2
case class Id(name: String) extends ScalarExpr // x

// Boolean Expressions
trait BoolExpr extends miniCAST // B ::=
case class Bool(bool: Boolean) extends BoolExpr // b
case class Eq(left: Id, right: Num) extends BoolExpr // x == n
case class Lt(left: Id, right: Num) extends BoolExpr // x < n

// Commands
trait Cmd extends miniCAST // C ::=
case class Skip() extends Cmd // skip;
case class Seq(head: Cmd, tail: Cmd) extends Cmd // C; C
case class Assign(name: String, expr: ScalarExpr) extends Cmd // x := E
case class In(name: String) extends Cmd // input(x)
case class Branch(cond: BoolExpr, trueBranch: Cmd, falseBranch: Cmd) extends Cmd // if(be){c}else{c}
case class While(cond: BoolExpr, body: Cmd) extends Cmd // while(b){c}

// Labelled AST of miniC
trait miniCLabelAST extends miniCAST {
    def getLabel(cmdL: CmdL): Int = cmdL match {
        case SkipL(label) => label
        case SeqL(label, _, _) => label
        case AssignL(label, _, _) => label
        case InL(label, _) => label
        case BranchL(label, _, _, _) => label
        case WhileL(label, _, _) => label
        case SOP(label) => label
        case EOP(label) => label
    }

    // Pretty Printer
    def pretty(cmd: CmdL, indent: String): String = cmd match {
        case SkipL(label) => indent + s"Skip [$label]"
        case SeqL(label, head, tail) => pretty(head, indent) + ";\n" + pretty(tail, indent)
        case AssignL(label, name, expr) => indent + name + " := " + pretty(expr) + s" [$label]"
        case InL(label, name) => indent + "input(" + name + ")" + s" [$label]"
        case BranchL(label, cond, trueBranch, falseBranch) => indent + "if(" + pretty(cond) + ")" + s" [$label]\n" + indent + "{\n" + pretty(trueBranch, indent + "   ") + "\n" + indent + "}\n" + indent + "else {\n" + pretty(falseBranch, indent + "   ") + "\n" + indent + "}"
        case WhileL(label, cond, body) => indent + "while(" + pretty(cond) + ")" + s" [$label]\n" + indent + "{\n" + pretty(body, indent + "   ") + "\n" + indent + "}"
        case SOP(label) => s"SOP [$label]"
        case EOP(label) => s"EOP [$label]"
    }
    def pretty(prog: ProgramL): String = {
        pretty(prog.cmd, "")
    }
}

// Mini C Program
case class ProgramL(cmd: CmdL) extends miniCLabelAST

// Commands
trait CmdL extends miniCLabelAST // C ::=
case class SkipL(label: Int) extends CmdL // skip;
case class SeqL(label: Int, head: CmdL, tail: CmdL) extends CmdL // C; C
case class AssignL(label: Int, name: String, expr: ScalarExpr) extends CmdL // x := E
case class InL(label: Int, name: String) extends CmdL // input(x)
case class BranchL(label: Int, cond: BoolExpr, trueBranch: CmdL, falseBranch: CmdL) extends CmdL // if(be){c}else{c}
case class WhileL(label: Int, cond: BoolExpr, body: CmdL) extends CmdL // while(b){c}
case class SOP(label: Int) extends CmdL
case class EOP(label: Int) extends CmdL