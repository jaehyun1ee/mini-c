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
            case EOP(_) => nextMap
        }

        val eop = progL.cmdL match {
            case SeqL(_, _, eop) => eop
            case _ => ???
        }
        nextCmdL(progL.cmdL, Map[(CmdL, Option[Boolean]), CmdL](), eop)
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
        println("a. Labelled AST")
        println(pretty(progL, nextMap) + "\n")

        // b, Define state transition
        // i.e., defining the step function
        def lookUpNext(cmdL: CmdL, opt: Option[Boolean]): CmdL = nextMap.getOrElse((cmdL, opt), interpError(s"no next ${cmdL.toString}"))
        def step(cmdL: CmdL, envT: EnvT): (CmdL, EnvT) = cmdL match {
            case SkipL(_) => (lookUpNext(cmdL, None), envT)
            case SeqL(_, _, _) => (lookUpNext(cmdL, None), envT)
            case AssignL(_, name, expr) => (lookUpNext(cmdL, None), envT + (name -> interpTrans(expr, envT)))
            case InL(label, name) => {
                print(s"[$label] Enter input for $name: ")
                (lookUpNext(cmdL, None), envT + (name -> readLine().toInt))
            }
            case BranchL(_, cond, _, _) => {
                if(interpTrans(cond, envT)) (lookUpNext(cmdL, Some(true)), envT)
                else (lookUpNext(cmdL, Some(false)), envT)
            }
            case WhileL(_, cond, _) => {
                if(interpTrans(cond, envT)) (lookUpNext(cmdL, Some(true)), envT)
                else (lookUpNext(cmdL, Some(false)), envT)
            }
            case SOP(_) => (lookUpNext(cmdL, None), envT)
            case EOP(_) => (cmdL, envT)
        }

        // c. Applying step transition for each state
        def transfer(state: State): State = state.map(x => {
            val (cmdL, env) = x
            step(cmdL, env)
        }) diff state
        def collect(prev: State, acc: State): State = {
            val cur = transfer(prev)
            if (cur.isEmpty) acc
            else collect(cur, acc union cur)
        }

        val initState = transfer(Set((progL.cmdL, Map[String, Int]())))
        val finalState = collect(initState, initState)

        println("b. Result of Transitional Interpreter")
        println(pretty(progL, nextMap, finalState) + "\n")
        finalState
    }

    // Pretty printer
    def pretty(cmdL: CmdL, indent: String, nextMap: NextMap): String = {
        def lookUpNext(cmdL: CmdL, opt: Option[Boolean]): Int = getLabel(nextMap.getOrElse((cmdL, opt), interpError(s"no next ${cmdL.toString}")))
        cmdL match {
            case SkipL(label) => indent + s"Skip [$label]=>[${lookUpNext(cmdL, None)}]"
            case SeqL(label, head, tail) => pretty(head, indent, nextMap) + "\n" + pretty(tail, indent, nextMap)
            case AssignL(label, name, expr) => indent + name + " := " + pretty(expr) + s" [$label]=>[${lookUpNext(cmdL, None)}]"
            case InL(label, name) => indent + "input(" + name + ")" + s" [$label]=>[${lookUpNext(cmdL, None)}]"
            case BranchL(label, cond, trueBranch, falseBranch) => indent + "if(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(cmdL, Some(true))}] f[${lookUpNext(cmdL, Some(false))}]\n" + indent + "{\n" + pretty(trueBranch, indent + "   ", nextMap) + "\n" + indent + "}\n" + indent + "else {\n" + pretty(falseBranch, indent + "   ", nextMap) + "\n" + indent + "}"
            case WhileL(label, cond, body) => indent + "while(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(cmdL, Some(true))}] f[${lookUpNext(cmdL, Some(false))}]\n" + indent + "{\n" + pretty(body, indent + "   ", nextMap) + "\n" + indent + "}"
            case SOP(label) => s"SOP [$label]=>[${lookUpNext(cmdL, None)}]"
            case EOP(label) => s"EOP [$label]"
        }
    }
    def pretty(progL: ProgramL, nextMap: NextMap): String = {
        pretty(progL.cmdL, "", nextMap)
    }
    def pretty(cmdL: CmdL, indent: String, nextMap: NextMap, stateMap: Map[Int, String]): String = {
        def lookUpNext(cmdL: CmdL, opt: Option[Boolean]): Int = getLabel(nextMap.getOrElse((cmdL, opt), interpError(s"no next ${cmdL.toString}")))
        def lookUpState(label: Int) = stateMap.getOrElse(label, "Unreached")
        cmdL match {
            case SkipL(label) => indent + s"Skip [$label]=>[${lookUpNext(cmdL, None)}]" + s"\t // ${lookUpState(label)}"
            case SeqL(label, head, tail) => pretty(head, indent, nextMap, stateMap) + "\n" + pretty(tail, indent, nextMap, stateMap)
            case AssignL(label, name, expr) => indent + name + " := " + pretty(expr) + s" [$label]=>[${lookUpNext(cmdL, None)}]" + s"\t // ${lookUpState(label)}"
            case InL(label, name) => indent + "input(" + name + ")" + s" [$label]=>[${lookUpNext(cmdL, None)}]" + s"\t // ${lookUpState(label)}"
            case BranchL(label, cond, trueBranch, falseBranch) => indent + "if(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(cmdL, Some(true))}] f[${lookUpNext(cmdL, Some(false))}]" + s"\t // ${lookUpState(label)}\n" + indent + "{\n" + pretty(trueBranch, indent + "   ", nextMap, stateMap) + "\n" + indent + "}\n" + indent + "else {\n" + pretty(falseBranch, indent + "   ", nextMap, stateMap) + "\n" + indent + "}"
            case WhileL(label, cond, body) => indent + "while(" + pretty(cond) + ")" + s" [$label]=>t[${lookUpNext(cmdL, Some(true))}] f[${lookUpNext(cmdL, Some(false))}]" + s"\t // ${lookUpState(label)}\n" + indent + "{\n" + pretty(body, indent + "   ", nextMap, stateMap) + "\n" + indent + "}"
            case SOP(label) => s"SOP [$label]=>[${lookUpNext(cmdL, None)}]" + s"\t // ${lookUpState(label)}"
            case EOP(label) => s"EOP [$label]" + s"\t // ${lookUpState(label)}"
        }
    }
    def pretty(progL: ProgramL, nextMap: NextMap, state: State): String = {
        val stateMap = state.map(x => {
            val (cmdL, envT) = x
            (getLabel(cmdL), envT.toString)
        }).groupBy(x => {
            val (label, envT) = x
            label
        }).map{case (label, envS) => {
            var res = ""
            envS.foreach(x => {
                val (label, envT) = x
                res = res + envT + ", "
            })
            (label, res)
        }}

        pretty(progL.cmdL, "", nextMap, stateMap)
    }
}