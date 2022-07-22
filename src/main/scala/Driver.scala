package miniC

trait miniCDriver extends miniCParser with miniCCompInterpreter with miniCTransInterpreter with miniCAnalyzerInterval
with comparator{
    def run(code: String): Unit = {
        val p: Program = ProgramParser(code)

        println("\n0. Code")
        println(code)
        println("\n1. Transitional Style")
        val state = interpTrans(p)
        println("\n2. Compositional Style")
        println(interpComp(p))
        println("\n3. (Flow-sensitive) Analysis")
        val absState = analyze_interval(p)
        diff(absState, state)
    }
}