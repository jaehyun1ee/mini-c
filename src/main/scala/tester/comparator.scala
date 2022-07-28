package miniC

import miniC.MiniCParser.parse

// TODO: Use MiniCTransInterpreter, MiniCAnalyzer(Interval) Objects instead of extension.
//  - the the code should somehow support type aliases outside the trait/class definition.
trait Comparator extends MiniCTransInterpreter with MiniCAnalyzer {
    def mergeState(state: State): Map[CmdL, Set[TransEnv]] = {
        state.groupBy((x: (CmdL, TransEnv)) => x._1).map{
            case (cmdL, stepRes) => cmdL -> stepRes.map(x => x._2)}
    }

    // TODO: Abstract this function for Parity and Sign
    def isContain(absEnv: AbsEnv, env: TransEnv): Boolean = {
        env.foldLeft(true){ case (res: Boolean, (key, value)) => {
            val range = absEnv.getOrElse(key, interpError(s"free identifier: $key"))
            range match {
                case Interval(a, b) => {
                    if(a <= value && value <= b) res && true
                    else res && false
                }
                case IntTop() => res && true
                case IntBottom() => res && false
            }
        }}
    }

    def diff(absState: AbsState, state: State): Boolean = {
        val mergedState = mergeState(state)
        val cmds = absState.keySet union mergedState.keySet
        var res = true;
        for(cmd <- cmds) {
            (absState.get(cmd), mergedState.get(cmd)) match {
                case ((Some(absEnv), Some(envS))) => {
                    println(cmd)
                    res = res && (envS.foldLeft(true) ((res, env) => {
                        res && isContain(absEnv, env)
                    }))
                }
                case _ => res = res
            }   
        }

        println(res)
        res
    }
}

/*
class ComparatorParity extends Comparator with Parity
object ComparatorParity extends ComparatorParity {
    def compare(code: String): Boolean = {
        val program = parse(code)
        diff(analyze(program), interpTrans(program))
    }
}
*/

class ComparatorInterval extends Comparator with IntervalDomain
object ComparatorInterval extends ComparatorInterval {
    def compare(code: String): Boolean = {
        val program = parse(code)
        diff(analyze(program), interpTrans(program))
    }
}

/*
class ComparatorSign extends Comparator with Sign
object ComparatorSign extends ComparatorSign {
    def compare(code: String): Boolean = {
        val program = parse(code)
        diff(analyze(program), interpTrans(program))
    }
}
*/