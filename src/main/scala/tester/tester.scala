package miniC

import scala.io.Source

object Tester extends miniCParser with miniCTransInterpreter with miniCAnalyzerInterval with comparator{
    def test(code: String): Boolean = {
        val p: Program = ProgramParser(code)
        val absState = analyze_interval(p)
        true
    }

    def start(): Unit = {
        val filename = "tests"
        for(line <- Source.fromFile(filename).getLines) {
            val result = test(line)
            println(result)
        }
    }    
}