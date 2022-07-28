package miniC

import miniC.MiniCParser.parse
import miniC.MiniCCompInterpreter.{interp => interpComp}
import miniC.MiniCTransInterpreter.{interp => interpTrans}
import miniC.MiniCAnalyzerParity.{analyze => analyzeParity}
import miniC.MiniCAnalyzerInterval.{analyze => analyzeInterval}
import miniC.MiniCAnalyzerSign.{analyze => analyzeSign}

object Driver {
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