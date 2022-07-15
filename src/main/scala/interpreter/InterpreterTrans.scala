package miniC

import scala.io.StdIn.readLine

// Compositional Interpreter of miniC
trait miniCTransInterpreter extends miniCLabelAST with miniCError {
    // TODO: How to deal with duplicate commands?
    // Make a labelled AST out of an AST
    // i.e., defining the label function
    def labelProg(prog: Program): (ProgramL, Int) = {
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
        val (cmdL, lastLabel) = labelCmd(prog.cmd, 0)
        (ProgramL(cmdL), lastLabel)
    }

    // Collect the execution order
    // i.e., defining the next function
    
    // Assign labels to commands
    // i.e., defining the label function
    /*
    type LabelMap = Map[Cmd, Int]
    def labelProg(prog: Program): (LabelMap, Int) = {
        def labelThis(cmd: Cmd, acc: (LabelMap, Int)): (LabelMap, Int) = {
            val (labelMap, label) = acc
            (labelMap + (cmd -> label), label + 1)
        }
        def labelCmd(cmd: Cmd, acc: (LabelMap, Int)): (LabelMap, Int) = cmd match {
            case Skip() => labelThis(cmd, acc)
            case Seq(head, tail) => {
                val (labelMapThis, labelOfThis): (LabelMap, Int) = labelThis(cmd, acc)
                labelCmd(tail, labelCmd(head, (labelMapThis, labelOfThis - 1)))
            }
            case Assign(_, _) => labelThis(cmd, acc)
            case In(_) => labelThis(cmd, acc)
            case Branch(_, trueBranch, falseBranch) => labelCmd(falseBranch, labelCmd(trueBranch, labelThis(cmd, acc)))
            case While(_, body) => labelCmd(body, labelThis(cmd, acc))
        }
        labelCmd(prog.cmd, (Map[Cmd, Int](), 0))
    }
    */

    // Collect the execution order
    // i.e., defining the next function
    /*
    type NextMap = Map[(Int, Option[Boolean]), Int]
    def nextProg(prog: Program): NextMap = {
        val (labelMap, endLabel) = labelProg(prog)

        def lookUpLabel(cmd: Cmd): Int = labelMap.getOrElse(cmd, interpError(cmd.toString))
        def nextThis(cmd: Cmd, nextMap: NextMap, next: Int, opt: Option[Boolean]): NextMap = nextMap + ((lookUpLabel(cmd), opt) -> next)
        def nextCmd(cmd: Cmd, nextMap: NextMap, next: Int): NextMap = cmd match {
            case Skip() => nextThis(cmd, nextMap, next, None)
            case Seq(head, tail) => nextCmd(tail, nextCmd(head, nextMap, lookUpLabel(tail)), next)
            case Assign(_, _) => nextThis(cmd, nextMap, next, None)
            case In(_) => nextThis(cmd, nextMap, next, None)
            case Branch(_, trueBranch, falseBranch) => {
                val nextMapCondTrue = nextThis(cmd, nextMap, lookUpLabel(trueBranch), Some(true))
                val nextMapCondFalse = nextThis(cmd, nextMapCondTrue, lookUpLabel(falseBranch), Some(false))
                val nextMapTrue = nextCmd(trueBranch, nextMapCondFalse, next)
                val nextMapFalse = nextCmd(falseBranch, nextMapTrue, next)
                nextMapFalse
            }
            case While(_, body) => {
                val nextMapCondTrue = nextThis(cmd, nextMap, lookUpLabel(body), Some(true))
                val nextMapCondFalse = nextThis(cmd, nextMapCondTrue, next, Some(false))
                val nextMapBody = nextCmd(body, nextMapCondFalse, lookUpLabel(cmd))
                nextMapBody
            }
        }
        nextCmd(prog.cmd, Map[(Int, Option[Boolean]), Int](), endLabel)
    }
    */

    // Interpret the code by applying the step function until fixpoint is reached
    // i.e., get the collecting semantics and defining the step function

}