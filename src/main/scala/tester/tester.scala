package miniC

import scala.io.Source
import miniC.MiniCParser.parse
import miniC.MiniCTransInterpreter.{interp => interpTrans}
import miniC.MiniCAnalyzerInterval.{analyze => analyzeInterval}
import miniC.ComparatorInterval.compare

object Tester {
    def test(code: String): Boolean = compare(code)

    def start(): Unit = {
        val filename = "tests"
        for(line <- Source.fromFile(filename).getLines()) {
            test(line)
        }
    }    
}