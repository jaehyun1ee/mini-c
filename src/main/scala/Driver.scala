package miniC

import miniC.miniCParser.parse
import miniC.miniCCompInterpreter.{interp => interpComp}
import miniC.miniCTransInterpreter.{interp => interpTrans}
import miniC.miniCAnalyzerParity.{analyze => analyzeParity}
import miniC.miniCAnalyzerInterval.{analyze => analyzeInterval}
import miniC.miniCAnalyzerSign.{analyze => analyzeSign}

trait miniCDriver {
    def run(code: String): Unit = {
        val program = parse(code)

        println("/*\n\tCode\n*/\n")
        println(code)
        println()

        println("/*\n\tInterpret (Compositional) Result\n*/\n")
        println(interpComp(program))
        println()

        println("/*\n\tInterpret (Compositional) Result\n*/\n")
        interpTrans(program)
        println()

        println("/*\n\tAnalyze (Parity) Result\n*/\n")
        analyzeParity(program)
        println()

        println("/*\n\tAnalyze (Interval) Result\n*/\n")
        analyzeInterval(program)
        println()

        println("/*\n\tAnalyze (Sign) Result\n*/\n")
        analyzeSign(program)
        println()
    }
}