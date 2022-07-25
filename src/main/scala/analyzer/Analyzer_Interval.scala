package miniC

// Flow-sensitive Static Analysis
trait miniCAnalyzerInterval extends miniCControlFlow with miniCError with IntervalDomain {
    /*
        Abstract environment and Abstract State
            - abstract environment: id(string) => abstract value
            - abstract state: command => abstract environment (at that command)
    */

    type AbsEnv = Map[String, IntervalDomain]
    type AbsState = Map[CmdL, AbsEnv]

    /*
        Flow-sensitive Static Analysis
    */

    def analyze(scalarExpr: ScalarExpr, env: AbsEnv): IntervalDomain = scalarExpr match {
        case Num(n) => Interval(n, n)
        case Add(left, right) => analyze(left, env) + analyze(right, env)
        case Sub(left, right) => analyze(left, env) - analyze(right, env)
        case Id(x) => env.getOrElse(x, analyzeError(s"free identifier: $x"))
    }
    def analyze(boolExpr: BoolExpr, env: AbsEnv): Option[Boolean] = boolExpr match {
        case Bool(b) => Some(b)
        case Eq(left, right) => analyze(left, env) same analyze(right, env)
        case Lt(left, right) => (analyze(left, env), analyze(right, env)) match {
            case (Interval(a, b), Interval(c, d)) => {
                if(b < c) Some(true)
                else if(d <= a) Some(false)
                else None
            }
            case _ => None
        }
    }
    def analyze(cmd: CmdL, env: AbsEnv, nextMap: NextMap): Set[(CmdL, AbsEnv)] = {
        def lookUpNext(opt: Option[Boolean]): CmdL = nextMap.getOrElse((cmd, opt), interpError(s"no next ${cmd.toString}"))
        cmd match {
            case SkipL(_) => Set((lookUpNext(None), env))
            case SeqL(_, _, _) => Set((lookUpNext(None), env))
            case AssignL(_, name, expr) => Set((lookUpNext(None), env + (name -> analyze(expr, env))))
            case InL(label, name) => Set((lookUpNext(None), env + (name -> IntTop())))
            case BranchL(_, cond, _, _) => analyze(cond, env) match {
                case Some(true) => Set((lookUpNext(Some(true)), env))
                case Some(false) => Set((lookUpNext(Some(false)), env))
                case None => Set((lookUpNext(Some(true)), env), (lookUpNext(Some(false)), env))
            }
            case WhileL(_, cond, _) => analyze(cond, env) match {
                case Some(true) => Set((lookUpNext(Some(true)), env))
                case Some(false) => Set((lookUpNext(Some(false)), env))
                case None => Set((lookUpNext(Some(true)), env), (lookUpNext(Some(false)), env))
            }
            case SOP(_) => Set((lookUpNext(None), env))
            case EOP(_) => Set.empty[(CmdL, AbsEnv)]
        }
    }
    // Abstract state collection framework
    // i.e., Apply step function pointwise, partition by label, then union the abstract states for each label
    def analyze(prog: ProgramL, nextMap: NextMap): AbsState = {
        /*
            Helper Functions
        */

        // From Map[CmdL, Map[String, IntervalDomain]] To Set[(CmdL, Map[String, IntervalDomain])]
        def pointwise(state: AbsState): Set[(CmdL, AbsEnv)] = state.toSet.map((x: (CmdL, AbsEnv)) => {
            val (cmd, env) = x
            analyze(cmd, env, nextMap)
        }).foldLeft(Set[(CmdL, AbsEnv)]()) ((x, y) => x union y)

        // From Set[(CmdL, Map[String, IntervalDomain])] To Map[CmdL, Set[Map[String, IntervalDomain]]]
        def partition(pointwiseState: Set[(CmdL, AbsEnv)]): Map[CmdL, Set[AbsEnv]] = pointwiseState.groupBy((x: (CmdL, AbsEnv)) => x._1).map{case (cmdL, stepRes) => cmdL -> stepRes.map(x => x._2)}
        
        // From Map[CmdL, Set[Map[String, IntervalDomain]]] To Map[CmdL, Map[String, IntervalDomain]]
        def union(partitionState: Map[CmdL, Set[AbsEnv]]): AbsState = partitionState.map{case (cmdL, envS) => {
            // From Set[Map[String, IntervalDomain]] To Set[(String, IntervalDomain)]
            val flat = envS.map(x => x.toSet).foldLeft(Set[(String, IntervalDomain)]()) ((x, y) => x union y)

            // From Set[(String, IntervalDomain)] To Map[String, Set[(String, IntervalDomain)]]
            val transpose = flat.groupBy(x => x._1)

            // From Map[String, Set[(String, IntervalDomain)]] To Map[String, IntervalDomain]
            val acc = transpose.map{case (id, envS) => {
                var base: IntervalDomain = IntBottom()
                val fold = envS.map(x => x._2).foldLeft(Set[IntervalDomain]()) ((x, y) => x + y).foldLeft(base) ((x, y) => x union y)
                id -> fold
            }}
            
            // To Map[CmdL, Map[String, IntervalDomain]]
            cmdL -> acc
        }}

        // Check if LEFT is different from RIGHT (i.e., LEFT is NOT contained in RIGHT)
        def diff(left: AbsState, right: AbsState): Boolean = {
            // First compute a naive difference by set diff operator
            val naiveDiff = left.toSet diff right.toSet

            // For unchecked cases in naive difference, check for abstract value containment
            val pointwiseDiff = naiveDiff.map(x => {
                val (cmdL, leftEnv) = x
                right.get(cmdL) match {
                    case Some(rightEnv) => leftEnv.map{case (id, leftVal) => rightEnv.get(id) match {
                        case Some(rightVal) => !(rightVal contain leftVal)
                        case None => true
                    }}.foldLeft(false) (_ || _)
                    case None => true
                }            
            })

            pointwiseDiff.foldLeft(false) (_ || _)
        }

        /* 
            Driver
        */

        def transfer(state: AbsState): AbsState = {     
            val pointwiseState = pointwise(state)
            val partitionState = partition(pointwiseState)
            val unionState = union(partitionState)
            
            unionState
        }

        def collect(prev: AbsState, acc: AbsState): AbsState = {
            val cur = transfer(prev)

            if(diff(cur, acc)) {
                val pointwiseState = cur.toSet union acc.toSet
                val partitionState = partition(pointwiseState)
                val unionState = union(partitionState)
                collect(cur, unionState)
            } else acc
        }

        // Run the framework
        val initState = transfer(Map(prog.cmd -> Map[String, Interval]()))
        val finalState = collect(initState, initState)
        println("b. Analysis Result")
        println(pretty(prog, nextMap, finalState))

        finalState
    }
    def analyze_interval(prog: Program): AbsState = {
         // Turn the program into a labelled program, and get the next mappings
        val labelProg = label(prog)
        val nextMap = next(labelProg)

        // Run the analysis
        analyze(labelProg, nextMap)
    }

    /* 
        Pretty Printers for Analysis Result
    */

    def pretty(prog: ProgramL, nextMap: NextMap, state: AbsState): String = {
        val stateMap = state.map{case (cmd, env) => getLabel(cmd) -> env.toString}
        pretty(prog.cmd, "", nextMap, stateMap)
    }
}