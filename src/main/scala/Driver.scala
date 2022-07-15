package miniC

trait miniCDriver extends miniCParser with miniCCompInterpreter with miniCTransInterpreter {
    def run(code: String): String = {
        val p: Program = ProgramParser(code)
        println(p.toString)
        println(labelProg(p).toString)
        //println(nextProg(p).toString)
        interpComp(p).toString
    }
}