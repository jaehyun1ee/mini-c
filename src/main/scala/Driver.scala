package miniC

trait miniCDriver extends miniCParser with miniCCompInterpreter with miniCTransInterpreter {
    def run(code: String): String = {
        val p: Program = ProgramParser(code)
        println(p.toString)

        val l = labelProg(p)
        println(l.toString)
        val n = nextProg(l)
        println(n.toString)
        println(interpComp(p).toString)
        interpTrans(p).toString
    }
}