package miniC

trait comparator extends miniCControlFlow with miniCAnalyzerInterval with miniCTransInterpreter{

    def mergeState(state: State): Map[CmdL, Set[TransEnv]] = {
        state.groupBy((x: (CmdL, TransEnv)) => x._1).map{
            case (cmdL, stepRes) => cmdL -> stepRes.map(x => x._2)}
    }

    def isContain(absEnv: AbsEnv, env:TransEnv):Boolean = {
        env.foldLeft(true){ case (res:Boolean, (key, value)) => {
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
        val manufacturedState = mergeState(state)
        val res = (absState.values, manufacturedState.values).zipped.foldLeft(true){
            case (result, (absEnv, envs)) => {
                result && (envs.foldLeft(true)((res, env)=>{
                    res && isContain(absEnv, env)
                }))
            }
        }
        println(res)
        res
    }
}