package miniC

import scala.io.StdIn.readLine

// Compositional Interpreter of miniC
trait miniCCompInterpreter extends miniCAST with miniCError {
    // Environment
    type Env = Map[String, Int]

    // Interpreter
    def interpComp(scalarExpr: ScalarExpr, env: Env): Int = scalarExpr match {
        case Num(num) => num
        case Add(left, right) => interpComp(left, env) + interpComp(right, env)
        case Sub(left, right) => interpComp(left, env) - interpComp(right, env)
        case Id(x) => lookup(x, env)
    }
    def interpComp(boolExpr: BoolExpr, env: Env): Boolean = boolExpr match {
        case Bool(bool) => bool
        case Eq(left, right) => interpComp(left, env) == interpComp(right, env) // TODO: left should be Id, right should be Num. How to enforce?
        case Lt(left, right) => interpComp(left, env) < interpComp(right, env) // TODO: left should be Id, right should be Num. How to enforce?
    }
    def interpComp(cmd: Cmd, env: Env): Env = cmd match {
        case Skip() => env
        case Seq(head, tail) => interpComp(tail, interpComp(head, env))
        case Assign(x, e) => env + (x -> interpComp(e, env))
        case In(x) => env + {
            print(s"Enter input for $x: ")
            (x -> readLine().toInt)
        }
        case Branch(cond, trueBranch, falseBranch) => if(interpComp(cond, env)) interpComp(trueBranch, env) else interpComp(falseBranch, env)
        case While(cond, body) => if(interpComp(cond, env)) interpComp(cmd, interpComp(body, env)) else env
    }
    def interpComp(prog: Program): Env = interpComp(prog.cmd, Map[String, Int]())
    
    def lookup(name: String, env: Env): Int = env.getOrElse(name, interpError(s"free identifier: $name"))
}