package miniC

// AST of miniC
trait miniCAST {
    // Mini C Program
    case class Program(cmd: Cmd)

    // Scalar Expressions
    trait ScalarExpr // E ::=
    case class Num(num: Int) extends ScalarExpr   // n
    case class Add(left: ScalarExpr, right: ScalarExpr) extends ScalarExpr // e1 + e2
    case class Sub(left: ScalarExpr, right: ScalarExpr) extends ScalarExpr // e1 - e2
    case class Id(name: String) extends ScalarExpr // x

    // Boolean Expressions
    trait BoolExpr // B ::=
    case class Bool(bool: Boolean) extends BoolExpr // b
    case class Eq(left: Id, right: Num) extends BoolExpr // x == n
    case class Lt(left: Id, right: Num) extends BoolExpr // x < n

    // Commands
    trait Cmd // C ::=
    case class Skip() extends Cmd // skip;
    case class Seq(head: Cmd, tail: Cmd) extends Cmd // C; C
    case class Assign(name: String, expr: ScalarExpr) extends Cmd // x := E
    case class In(name: String) extends Cmd // input(x)
    case class Branch(cond: BoolExpr, trueBranch: Cmd, falseBranch: Cmd) extends Cmd // if(be){c}else{c}
    case class While(cond: BoolExpr, body: Cmd) extends Cmd // while(b){c}
}

// Labelled AST of miniC
trait miniCLabelAST extends miniCAST {
    // Mini C Program
    case class ProgramL(cmdL: CmdL)

    // Commands
    trait CmdL // C ::=
    case class SkipL(label: Int) extends CmdL // skip;
    case class SeqL(label: Int, head: CmdL, tail: CmdL) extends CmdL // C; C
    case class AssignL(label: Int, name: String, expr: ScalarExpr) extends CmdL // x := E
    case class InL(label: Int, name: String) extends CmdL // input(x)
    case class BranchL(label: Int, cond: BoolExpr, trueBranch: CmdL, falseBranch: CmdL) extends CmdL // if(be){c}else{c}
    case class WhileL(label: Int, cond: BoolExpr, body: CmdL) extends CmdL // while(b){c}
    case class SOP(label: Int) extends CmdL
    case class EOP(label: Int) extends CmdL
}