package miniC

trait miniCDriver extends miniCParser with miniCCompInterpreter with miniCTransInterpreter {
    def run(code: String): String = {
        val p: Program = ProgramParser(code)

        println("1. Transitional Style")
        interpTrans(p)
        println("2. Compositional Style")
        interpComp(p).toString
    }
}