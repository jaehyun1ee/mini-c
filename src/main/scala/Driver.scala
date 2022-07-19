package miniC

trait miniCDriver extends miniCParser with miniCCompInterpreter with miniCTransInterpreter with miniCAnalyzer {
    def run(code: String): Unit = {
        val p: Program = ProgramParser(code)

        println("\n0. Code")
        println(code)
        println("\n1. Transitional Style")
        interpTrans(p)
        println("\n2. Compositional Style")
        println(interpComp(p))
        println("\n3. (Flow-sensitive) Analysis")
        analyze(p)
    }
}