package miniC

// Extract Control Flow from AST
trait miniCControlFlow extends miniCLabelAST {
    /*
        Labelling a Given AST
            - Make a labelled AST out of an AST (to make each command unique)
                - i.e., defining the label function
    */

    def label(prog: Program): ProgramL = {
        def labelCmd(cmd: Cmd, label: Int): (CmdL, Int) = cmd match {
            case Skip() => (SkipL(label), label + 1)
            case Seq(head, tail) => {
                val (headCmd, headLabel) = labelCmd(head, label)
                val (tailCmd, tailLabel) = labelCmd(tail, headLabel)
                (SeqL(label, headCmd, tailCmd), tailLabel)
            }
            case Assign(name, expr) => (AssignL(label, name, expr), label + 1)
            case In(name) => (InL(label, name), label + 1)
            case Branch(cond, trueBranch, falseBranch) => {
                val (trueCmd, trueLabel) = labelCmd(trueBranch, label + 1)
                val (falseCmd, falseLabel) = labelCmd(falseBranch, trueLabel)
                (BranchL(label, cond, trueCmd, falseCmd), falseLabel)
            }
            case While(cond, body) => {
                val (bodyCmd, bodyLabel) = labelCmd(body, label + 1)
                (WhileL(label, cond, bodyCmd), bodyLabel)
            }
        }

        val (cmdL, lastLabel) = labelCmd(prog.cmd, 1)
        ProgramL(SeqL(0, SeqL(0, SOP(0), cmdL), EOP(lastLabel)))
    }

    /*
        Determine the (Static) Control Flow
            - Collect the execution order
                - i.e., defining the next function
    */

    type NextMap = Map[(CmdL, Option[Boolean]), CmdL]
    def next(prog: ProgramL): NextMap = {
        def nextCmdL(cmdL: CmdL, nextMap: NextMap, next: CmdL): NextMap = cmdL match {
            case SkipL(_) => nextMap + ((cmdL, None) -> next)
            case SeqL(_, head, tail) => nextCmdL(tail, nextCmdL(head, nextMap + ((cmdL, None) -> head), tail), next)
            case AssignL(_, _, _) => nextMap + ((cmdL, None) -> next)
            case InL(_, _) => nextMap + ((cmdL, None) -> next)
            case BranchL(_, _, trueBranch, falseBranch) => {
                val nextMapCondTrue = nextMap + ((cmdL, Some(true)) -> trueBranch)
                val nextMapCondFalse = nextMapCondTrue + ((cmdL, Some(false)) -> falseBranch)
                val nextMapTrue = nextCmdL(trueBranch, nextMapCondFalse, next)
                val nextMapFalse = nextCmdL(falseBranch, nextMapTrue, next)
                nextMapFalse
            }
            case WhileL(_, _, body) => {
                val nextMapCondTrue = nextMap + ((cmdL, Some(true)) -> body)
                val nextMapCondFalse = nextMapCondTrue + ((cmdL, Some(false)) -> next)
                val nextMapBody = nextCmdL(body, nextMapCondFalse, cmdL)
                nextMapBody
            }
            case SOP(_) => nextMap + ((cmdL, None) -> next)
            case EOP(_) => nextMap
        }

        val eop = prog.cmd match {
            case SeqL(_, _, eop) => eop
            case _ => ???
        }
        nextCmdL(prog.cmd, Map[(CmdL, Option[Boolean]), CmdL](), eop)
    }

    /*
        Pretty Printers for Labelling and Next Result
    */

    def pretty(cmd: CmdL, indent: String, nextMap: NextMap, commentMap: Map[Int, String], default: String): String = {
        def lookUpNext(opt: Option[Boolean]): Int = getLabel(nextMap.getOrElse((cmd, opt), EOP(-1)))
        def lookUpComment(label: Int) = commentMap.getOrElse(label, default)
        cmd match {
            case SkipL(label) => indent + s"Skip [$label]=>[${lookUpNext(None)}]" + s"\t // ${lookUpComment(label)}"
            case SeqL(label, head, tail) => pretty(head, indent, nextMap, commentMap, default) + "\n" + pretty(tail, indent, nextMap, commentMap, default)
            case AssignL(label, name, expr) => indent + name + " := " + pretty(expr) + s" [$label]=>[${lookUpNext(None)}]" + s"\t // ${lookUpComment(label)}"
            case InL(label, name) => indent + "input(" + name + ")" + s" [$label]=>[${lookUpNext(None)}]" + s"\t // ${lookUpComment(label)}"
            case BranchL(label, cond, trueBranch, falseBranch) => indent + "if(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(Some(true))}] f[${lookUpNext(Some(false))}]" + s"\t // ${lookUpComment(label)}\n" + indent + "{\n" + pretty(trueBranch, indent + "   ", nextMap, commentMap, default) + "\n" + indent + "}\n" + indent + "else {\n" + pretty(falseBranch, indent + "   ", nextMap, commentMap, default) + "\n" + indent + "}"
            case WhileL(label, cond, body) => indent + "while(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(Some(true))}] f[${lookUpNext(Some(false))}]" + s"\t // ${lookUpComment(label)}\n" + indent + "{\n" + pretty(body, indent + "   ", nextMap, commentMap, default) + "\n" + indent + "}"
            case SOP(label) => s"SOP [$label]=>[${lookUpNext(None)}]" + s"\t // ${lookUpComment(label)}"
            case EOP(label) => s"EOP [$label]" + s"\t // ${lookUpComment(label)}"
        }
    }
    def pretty(prog: ProgramL, nextMap: NextMap): String = {
        pretty(prog.cmd, "", nextMap, Map[Int, String](), "")
    }
}