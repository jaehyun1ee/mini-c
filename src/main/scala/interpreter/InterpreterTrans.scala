package miniC

import scala.io.StdIn.readLine

// Compositional Interpreter of miniC
trait miniCTransInterpreter extends miniCLabelAST with miniCError {
    // 1. Make a labelled AST out of an AST (to make each command unique)
    // i.e., defining the label function
    def labelProg(prog: Program): ProgramL = {
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

    // 2. Collect the execution order
    // i.e., defining the next function
    type NextMap = Map[(CmdL, Option[Boolean]), CmdL]
    def nextProg(progL: ProgramL): NextMap = {
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
            case EOP(_) => ???
        }

        val (cmdL, eop) = progL.cmdL match {
            case SeqL(_, cmdL, eop) => (cmdL, eop)
            case _ => ???
        }
        nextCmdL(cmdL, Map[(CmdL, Option[Boolean]), CmdL](), eop)
    }

    // 3. Interpret the code by applying the step function until fixpoint is reached
    // i.e., get the collecting semantics and defining the step function
    type EnvT = Map[String, Int]
    type State = Set[(CmdL, EnvT)]
    def interpTrans(scalarExpr: ScalarExpr, env: EnvT): Int = scalarExpr match {
        case Num(n) => n
        case Add(left, right) => interpTrans(left, env) + interpTrans(right, env)
        case Sub(left, right) => interpTrans(left, env) - interpTrans(right, env)
        case Id(x) => env.getOrElse(x, interpError(s"free identifier: $x"))
    }
    def interpTrans(boolExpr: BoolExpr, env: EnvT): Boolean = boolExpr match {
        case Bool(b) => b
        case Eq(left, right) => interpTrans(left, env) == interpTrans(right, env)
        case Lt(left, right) => interpTrans(left, env) < interpTrans(right, env)
    }
    def interpTrans(prog: Program): State = {
        // a. Turn the program into a labelled program, and get the next mappings
        val progL = labelProg(prog)
        val nextMap = nextProg(progL)

        // b, Define state transition
        // i.e., defining the  step function
        def lookUpNext(cmdL: CmdL, opt: Option[Boolean]): CmdL = nextMap.getOrElse((cmdL, opt), interpError(s"no next ${cmdL.toString}"))
        def step(cmdL: CmdL, env: EnvT): (CmdL, EnvT) = cmdL match {
            case SkipL(_) => (lookUpNext(cmdL, None), env)
            case SeqL(_, _, _) => (lookUpNext(cmdL, None), env)
            case AssignL(_, name, expr) => (lookUpNext(cmdL, None), env + (name -> interpTrans(expr, env)))
            case InL(_, name) => {
                print(s"Enter input for $name: ")
                (lookUpNext(cmdL, None), env + (name -> readLine().toInt))
            }
            case BranchL(_, cond, _, _) => {
                if(interpTrans(cond, env)) (lookUpNext(cmdL, Some(true)), env)
                else (lookUpNext(cmdL, Some(false)), env)
            }
            case WhileL(_, cond, _) => {
                if(interpTrans(cond, env)) (lookUpNext(cmdL, Some(true)), env)
                else (lookUpNext(cmdL, Some(false)), env)
            }
            case SOP(_) => (lookUpNext(cmdL, None), env)
            case EOP(_) => (cmdL, env)
        }

        // c. Applying step transition for each state
        def transfer(state: State): State = state.map(x => {
            val (cmdL, env) = x
            step(cmdL, env)
        }) diff state
        val cmdL = progL.cmdL match {
            case SeqL(_, cmdL, _) => cmdL
            case _ => ???
        }
        var loop = transfer(Set((cmdL, Map[String, Int]())))
        var state = Set[(CmdL, EnvT)]()
        var continue = true
        while(continue) {
            loop = transfer(loop)
            if(loop.isEmpty) continue = false
            else state = state union loop
        }
        /*
        while(continue) {
            val transferState = transfer(state)
            if(state == (state union transferState)) continue = false
            else state = state union transferState
        }
        */

        println(pretty(progL.cmdL, "", nextMap) + "\n")
        state
    }

    // Pretty printer
    def pretty(cmdL: CmdL, indent: String, nextMap: NextMap): String = {
        def lookUpNext(cmdL: CmdL, opt: Option[Boolean]): Int = nextMap.getOrElse((cmdL, opt), interpError(s"no next ${cmdL.toString}")) match {
            case SkipL(label) => label
            case SeqL(label, _, _) => label
            case AssignL(label, _, _) => label
            case InL(label, _) => label
            case BranchL(label, _, _, _) => label
            case WhileL(label, _, _) => label
            case SOP(label) => label
            case EOP(label) => label
        }
        cmdL match {
            case SkipL(label) => indent + s"Skip [$label]=>[${lookUpNext(cmdL, None)}]"
            case SeqL(label, head, tail) => pretty(head, indent, nextMap) + ";\n" + pretty(tail, indent, nextMap)
            case AssignL(label, name, expr) => indent + name + " := " + pretty(expr) + s" [$label]=>[${lookUpNext(cmdL, None)}]"
            case InL(label, name) => indent + "input(" + name + ")" + s" [$label]=>[${lookUpNext(cmdL, None)}]"
            case BranchL(label, cond, trueBranch, falseBranch) => indent + "if(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(cmdL, Some(true))}] f[${lookUpNext(cmdL, Some(false))}]\n" + indent + "{\n" + pretty(trueBranch, indent + "   ", nextMap) + "\n" + indent + "}\n" + indent + "else {\n" + pretty(falseBranch, indent + "   ", nextMap) + "\n" + indent + "}"
            case WhileL(label, cond, body) => indent + "while(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(cmdL, Some(true))}] f[${lookUpNext(cmdL, Some(false))}]\n" + indent + "{\n" + pretty(body, indent + "   ", nextMap) + "\n" + indent + "}"
            case SOP(label) => s"SOP [$label]=>[${lookUpNext(cmdL, None)}]"
            case EOP(label) => s"EOP [$label]"
        }
    }

    def pretty(state: State): String = {
        var res: String = ""
        state.foreach(x => {
            val (cmdL, envT) = x
            res = res + (cmdL match {
                case SkipL(label) => s"${label} => ${envT.toString}\n"
                case SeqL(_, _, _) => ""
                case AssignL(label, _, _) => s"${label} => ${envT.toString}\n"
                case InL(label, _) => s"${label} => ${envT.toString}\n"
                case BranchL(label, _, _, _) => s"${label} => ${envT.toString}\n"
                case WhileL(label, _, _) => s"${label} => ${envT.toString}\n"
                case EOP(label) => s"${label} => ${envT.toString}\n"
                case SOP(label) => s"${label} => ${envT.toString}\n"
            })
        })
        
        res
    }
}