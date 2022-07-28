package miniC

import miniC.Driver.run
import miniC.Grammar.genTest
import miniC.Tester.{start => startTest}

object Main extends App {
    genTest()
    startTest()
}
