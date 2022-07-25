package miniC

import scala.io.StdIn.readLine
import scala.util.Random._

// Compositional Interpreter of miniC
trait miniCTransInterpreter extends miniCControlFlow with miniCError {
    /*
        Define Environment and State
            - environment: id(string) => value(int)
            - state: command => environment (at that command)
    */

    type TransEnv = Map[String, Int]
    type State = Set[(CmdL, TransEnv)]

    /*
        Transitional-style Interpreter
    */

    // 3. Interpret the code by applying the step function until fixpoint is reached
    // i.e., get the collecting semantics and defining the step function
    def interpTrans(scalarExpr: ScalarExpr, env: TransEnv): Int = scalarExpr match {
        case Num(n) => n
        case Add(left, right) => interpTrans(left, env) + interpTrans(right, env)
        case Sub(left, right) => interpTrans(left, env) - interpTrans(right, env)
        case Id(x) => env.getOrElse(x, interpError(s"free identifier: $x"))
    }
    def interpTrans(boolExpr: BoolExpr, env: TransEnv): Boolean = boolExpr match {
        case Bool(b) => b
        case Eq(left, right) => interpTrans(left, env) == interpTrans(right, env)
        case Lt(left, right) => interpTrans(left, env) < interpTrans(right, env)
    }
    def interpTrans(cmd: CmdL, env: TransEnv, nextMap: NextMap): (CmdL, TransEnv) = {
        def lookUpNext(opt: Option[Boolean]): CmdL = nextMap.getOrElse((cmd, opt), interpError(s"no next ${cmd.toString}"))
        cmd match {
            case SkipL(_) => (lookUpNext(None), env)
            case SeqL(_, _, _) => (lookUpNext(None), env)
            case AssignL(_, name, expr) => (lookUpNext(None), env + (name -> interpTrans(expr, env)))
            case InL(label, name) => {
                (lookUpNext(None), env + (name -> nextInt(10)))
            }
            case BranchL(_, cond, _, _) => {
                if(interpTrans(cond, env)) (lookUpNext(Some(true)), env)
                else (lookUpNext(Some(false)), env)
            }
            case WhileL(_, cond, _) => {
                if(interpTrans(cond, env)) (lookUpNext(Some(true)), env)
                else (lookUpNext(Some(false)), env)
            }
            case SOP(_) => (lookUpNext(None), env)
            case EOP(_) => (cmd, env)
        }
    }
    def interpTrans(prog: ProgramL, nextMap: NextMap): State = {
        // Applying step transition for each state
        def transfer(state: State): State = state.map(x => {
            val (cmd, env) = x
            interpTrans(cmd, env, nextMap)
        }) diff state
        def collect(prev: State, acc: State): State = {
            val cur = transfer(prev)
            if (cur.isEmpty) acc
            else collect(cur, acc union cur)
        }

        // Run the interpreter
        val initState = transfer(Set((prog.cmd, Map[String, Int]())))
        val finalState = collect(initState, initState)
        println("b. Result of Transitional Interpreter")
        println(pretty(prog, nextMap, finalState) + "\n")

        finalState
    }
    def interpTrans(prog: Program): State = {
        // Turn the program into a labelled program, and get the next mappings
        val labelProg = label(prog)
        val nextMap = next(labelProg)
        // Run the interpreter
        interpTrans(labelProg, nextMap)
    }

    /*
        Pretty Printers for Interpret Result
    */

    def pretty(prog: ProgramL, nextMap: NextMap, state: State): String = {
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

        pretty(prog.cmd, "", nextMap, stateMap)
    }
}