package miniC

// Try analyzing in a flow-sensitive (transitive) style
trait miniCAnalyzer extends miniCAST with miniCTransInterpreter with miniCError {
    // Abstraction (parity analysis)
    trait Parity {
        def +(that: Parity): Parity = (this, that) match {
            case (Bottom(), Bottom()) => Bottom()
            case (Bottom(), Even()) => Even()
            case (Bottom(), Odd()) => Odd()
            case (Even(), Bottom()) => Even()
            case (Odd(), Bottom()) => Odd()
            case (Top(), _) => Top()
            case (_, Top()) => Top()
            case (Even(), Even()) => Even()
            case (Even(), Odd()) => Odd()
            case (Odd(), Even()) => Odd()
            case (Odd(), Odd()) => Even()
        }

        def -(that: Parity): Parity = this + that

        def same(that: Parity): Option[Boolean] = (this, that) match {
            case (Even(), Odd()) => Some(false)
            case (Odd(), Even()) => Some(false)
            case _ => None
        }

        def union(that: Parity): Parity = (this, that) match {
            case (Bottom(), Bottom()) => Bottom()
            case (Bottom(), Even()) => Even()
            case (Bottom(), Odd()) => Odd()
            case (Even(), Bottom()) => Even()
            case (Odd(), Bottom()) => Odd()
            case (Top(), _) => Top()
            case (_, Top()) => Top()
            case (Even(), Even()) => Even()
            case (Odd(), Odd()) => Odd()
            case (Even(), Odd()) => Top()
            case (Odd(), Even()) => Top()
        }

        // return true if THIS contains THAT
        def contain(that: Parity): Boolean = (this, that) match {
            case (Top(), Top()) => true
            case (_, Top()) => false
            case (Top(), _) => true
            case (Bottom(), Bottom()) => true
            case (_, Bottom()) => true
            case (Bottom(), _) => false
            case (Even(), Even()) => true
            case (Odd(), Odd()) => true
            case _ => false
        }
    }
    case class Bottom() extends Parity
    case class Even() extends Parity
    case class Odd() extends Parity
    case class Top() extends Parity

    // Analyze the code by the worklist algorithm
    type EnvA = Map[String, Parity]
    type StateA = Map[CmdL, EnvA]

    def diff(left: StateA, right: StateA): Boolean = {
        val naive = left.toSet diff right.toSet
        val pointwiseDiff = naive.map(x => {
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

    def analyze(scalarExpr: ScalarExpr, envA: EnvA): Parity = scalarExpr match {
        case Num(n) => if(n % 2 == 0) Even() else Odd()
        case Add(left, right) => analyze(left, envA) + analyze(right, envA)
        case Sub(left, right) => analyze(left, envA) - analyze(right, envA)
        case Id(x) => envA.getOrElse(x, analyzeError(s"free identifier: $x"))
    }
    def analyze(boolExpr: BoolExpr, envA: EnvA): Option[Boolean] = boolExpr match {
        case Bool(b) => Some(b)
        case Eq(left, right) => analyze(left, envA) same analyze(right, envA)
        case _ => None
    }
    def analyze(prog: Program): StateA = {
         // a. Turn the program into a labelled program, and get the next mappings
        val progL = labelProg(prog)
        val nextMap = nextProg(progL)
        println("a. Labelled AST")
        println(pretty(progL, nextMap) + "\n")

        // b, Define state transition
        // i.e., defining the abstract step function
        def lookUpNext(cmdL: CmdL, opt: Option[Boolean]): CmdL = nextMap.getOrElse((cmdL, opt), interpError(s"no next ${cmdL.toString}"))
        def step(cmdL: CmdL, envA: EnvA): Set[(CmdL, EnvA)] = cmdL match {
            case SkipL(_) => Set((lookUpNext(cmdL, None), envA))
            case SeqL(_, _, _) => Set((lookUpNext(cmdL, None), envA))
            case AssignL(_, name, expr) => Set((lookUpNext(cmdL, None), envA + (name -> analyze(expr, envA))))
            case InL(label, name) => Set((lookUpNext(cmdL, None), envA + (name -> Top())))
            case BranchL(_, cond, _, _) => analyze(cond, envA) match {
                case Some(true) => Set((lookUpNext(cmdL, Some(true)), envA))
                case Some(false) => Set((lookUpNext(cmdL, Some(false)), envA))
                case None => Set((lookUpNext(cmdL, Some(true)), envA), (lookUpNext(cmdL, Some(false)), envA)) // TODO
            }
            case WhileL(_, cond, _) => analyze(cond, envA) match {
                case Some(true) => Set((lookUpNext(cmdL, Some(true)), envA))
                case Some(false) => Set((lookUpNext(cmdL, Some(false)), envA))
                case None => Set((lookUpNext(cmdL, Some(true)), envA), (lookUpNext(cmdL, Some(false)), envA)) // TODO
            }
            case SOP(_) => Set((lookUpNext(cmdL, None), envA))
            case EOP(_) => Set.empty[(CmdL, EnvA)]
        }

        // c. Define framework
        // i.e., Apply step function pointwise, partition by label, then union the abstract states for each label
        def partition(pointwiseState: Set[(CmdL, EnvA)]): Map[CmdL, Set[EnvA]] = pointwiseState.groupBy((x: (CmdL, EnvA)) => x._1).map{case (cmdL, stepRes) => cmdL -> stepRes.map(x => x._2)}
        def union(partitionState: Map[CmdL, Set[EnvA]]): StateA = partitionState.map{case (cmdL, envS) => {
            // From Set[Map[String, Parity]] To Set[(String, Parity)]
            val flat = envS.map(x => x.toSet).foldLeft(Set[(String, Parity)]()) ((x, y) => x union y)
            // From Set[(String, Parity)] To Map[String, Set[(String, Parity)]]
            val transpose = flat.groupBy(x => x._1)
            // From Map[String, Set[(String, Parity)]] To Map[String, Parity]
            val acc = transpose.map{case (id, envS) => {
                var base: Parity = Bottom()
                val fold = envS.map(x => x._2).foldLeft(Set[Parity]()) ((x, y) => x + y).foldLeft(base) ((x, y) => x union y)
                id -> fold
            }}
            
            // To Map[CmdL, Map[String, Parity]]
            cmdL -> acc
        }}
        def transfer(stateA: StateA): StateA = {
            // From Map[CmdL, Map[String, Parity]] To Set[(CmdL, Map[String, Parity])]
            val pointwiseState = stateA.toSet.map((x: (CmdL, EnvA)) => {
                val (cmdL, envA) = x
                step(cmdL, envA)
            }).foldLeft(Set[(CmdL, EnvA)]()) ((x, y) => x union y)
            // From Set[(CmdL, Map[String, Parity])] To Map[CmdL, Set[Map[String, Parity]]]
            val partitionState = partition(pointwiseState)
            // From Map[CmdL, Set[Map[String, Parity]]] To Map[CmdL, Map[String, Parity]]
            val unionState = union(partitionState)
            
            unionState
        }
        def collect(prev: StateA, acc: StateA): StateA = {
            val cur = transfer(prev)

            if(diff(cur, acc)) {
                val pointwiseState = cur.toSet union acc.toSet
                val partitionState = partition(pointwiseState)
                val unionState = union(partitionState)
                collect(cur, unionState)
            } else acc
        }

        val initState = transfer(Map(progL.cmdL -> Map[String, Parity]()))
        val finalState = collect(initState, initState)

        println("b. Analysis Result")
        println(pretty(progL, nextMap, finalState))

        finalState
    }

    // Pretty printer
    override def pretty(cmdL: CmdL, indent: String, nextMap: NextMap, stateMap: Map[Int, String]): String = {
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
    def pretty(progL: ProgramL, nextMap: NextMap, stateA: StateA): String = {
        val stateMap = stateA.map{case (cmdL, envA) => getLabel(cmdL) -> envA.toString}

        pretty(progL.cmdL, "", nextMap, stateMap)
    }
}