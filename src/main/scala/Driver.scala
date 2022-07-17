package miniC

trait miniCDriver extends miniCParser with miniCCompInterpreter with miniCTransInterpreter {
    def run(code: String): String = {
        val p: Program = ProgramParser(code)

        println("Transitional Style\n")
        println(pretty(interpTrans(p)))
        println("Compositional Style\n")
        interpComp(p).toString
    }
}