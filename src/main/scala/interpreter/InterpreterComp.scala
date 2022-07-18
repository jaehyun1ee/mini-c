package miniC

import scala.io.StdIn.readLine

// Compositional Interpreter of miniC
trait miniCCompInterpreter extends miniCAST with miniCError {
    // Environment
    type EnvC = Map[String, Int]

    // Interpreter
    def interpComp(scalarExpr: ScalarExpr, envC: EnvC): Int = scalarExpr match {
        case Num(num) => num
        case Add(left, right) => interpComp(left, envC) + interpComp(right, envC)
        case Sub(left, right) => interpComp(left, envC) - interpComp(right, envC)
        case Id(x) => envC.getOrElse(x, interpError(s"free identifier: $x"))
    }
    def interpComp(boolExpr: BoolExpr, envC: EnvC): Boolean = boolExpr match {
        case Bool(bool) => bool
        case Eq(left, right) => interpComp(left, envC) == interpComp(right, envC) // TODO: left should be Id, right should be Num. How to enforce?
        case Lt(left, right) => interpComp(left, envC) < interpComp(right, envC) // TODO: left should be Id, right should be Num. How to enforce?
    }
    def interpComp(cmd: Cmd, envC: EnvC): EnvC = cmd match {
        case Skip() => envC
        case Seq(head, tail) => interpComp(tail, interpComp(head, envC))
        case Assign(x, e) => envC + (x -> interpComp(e, envC))
        case In(x) => envC + {
            print(s"Enter input for $x: ")
            (x -> readLine().toInt)
        }
        case Branch(cond, trueBranch, falseBranch) => if(interpComp(cond, envC)) interpComp(trueBranch, envC) else interpComp(falseBranch, envC)
        case While(cond, body) => if(interpComp(cond, envC)) interpComp(cmd, interpComp(body, envC)) else envC
    }
    def interpComp(prog: Program): EnvC = interpComp(prog.cmd, Map[String, Int]())    
}