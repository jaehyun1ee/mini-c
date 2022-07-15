package miniC

import scala.io.StdIn.readLine

// TODO: implement pretty printer to check if it is really correct
// TODO: dealing with input only once when applying step

// Compositional Interpreter of miniC
trait miniCTransInterpreter extends miniCLabelAST with miniCError {
    // Make a labelled AST out of an AST (to make each command unique)
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

    // Collect the execution order
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

    // Interpret the code by applying the step function until fixpoint is reached
    // i.e., get the collecting semantics and defining the step function
    // State is a map from a label to an environment at that label e.g. (1 => (x => (1, 2, 3)))
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
        val progL = labelProg(prog)
        val nextMap = nextProg(progL)

        // Define state transition
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
        def transfer(state: State): State = state.map(x => {
            val (cmdL, env) = x
            step(cmdL, env)
        })

        val cmdL = progL.cmdL match {
            case SeqL(_, cmdL, _) => cmdL
            case _ => ???
        }
        var state = transfer(Set((cmdL, Map[String, Int]())))
        var continue = true
        while(continue) {
            val transferState = transfer(state)
            if(state == (state union transferState)) continue = false
            else state = state union transferState
        }

        state
    }
}