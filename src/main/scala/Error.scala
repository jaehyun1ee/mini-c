package miniC

trait miniCError {
    case class ParseError(msg: String) extends Error(s"[PARSE ERROR] $msg")
    def parseError[T](msg: String): T = throw ParseError(msg)

    case class InterpError(msg: String) extends Error(s"[INTERPRET ERROR] $msg")
    def interpError[T](msg: String): T = throw InterpError(msg)

    case class AnalyzeError(msg: String) extends Error(s"[ANALYZE ERROR] $msg")
    def analyzeError[T](msg: String): T = throw AnalyzeError(msg)
}

