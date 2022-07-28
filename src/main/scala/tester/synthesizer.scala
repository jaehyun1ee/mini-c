package miniC

import scala.util.Random._
import java.io._
import scala.language.implicitConversions

object Grammar {

  // TestGenerator

  // TODO: update the random sampling algorithm
  def weightedRandomIndex(length: Int) = {
    val n = nextInt(length * 9)
    if (n >= length) 0 else n
  }

  val repetitions = "0" * 80 + "1" * 15 + "2" * 4 + "3"

  sealed trait TestGenerator {
    def ~(rhs: TestGenerator) = new ~(this, rhs)
    def ~(rhs: String) = new ~(this, rhs)

    // ****Important****
    // each toString call returns a different String
    override def toString: String = this match {
      case Normal(test) => test
      case Prod(name) =>
        if (name == "genId") "x"
        else {
          val candidate = PEG_Grammar(name)
          candidate(weightedRandomIndex(candidate.length))
        }
      case ~(a, b) => (s"$a", s"$b") match {
        case ("", str2) => str2
        case (str1, "") => str1
        case (str1, str2) => s"$str1 $str2"
      }
      // TODO: update repetition
      // 0 ~ 2 repetition
      case Rep(a) =>
        val times = repetitions.charAt(nextInt(100)) - 48
        (for (i <- 1 to times) yield a).mkString(" ")
    }

    def testWithDepth(depth: Int): String = this match {
      case Normal(test) => test
      case Prod(name) =>
        if (name == "genId") "x"
        else {
          val candidate = PEG_Grammar(name)
          val index = nextInt(candidate.length)
          if (depth == 0 || index == 0) candidate(0).testWithDepth(depth)
          else candidate(index).testWithDepth(depth - 1)
        }
      case ~(a, b) => (a.testWithDepth(depth), b.testWithDepth(depth)) match {
        case ("", str2) => str2
        case (str1, "") => str1
        case (str1, str2) => s"$str1 $str2"
      }
      case Rep(a) =>
        val times = nextInt(4)
        val seq = for (i <- 1 to times) yield a.testWithDepth(depth)
        seq.foldLeft(Normal(""): TestGenerator)((t, e) => t ~ e).testWithDepth(depth)
    }

  }

  case class Normal(test: String) extends TestGenerator

  case class Prod(name: String) extends TestGenerator

  case class ~(a: TestGenerator, b: TestGenerator) extends TestGenerator

  case class Rep(a: TestGenerator) extends TestGenerator

  def Rep1(a: TestGenerator): TestGenerator = a ~ Rep(a)

  def Opt(t: TestGenerator): TestGenerator =
    if (nextInt(5) == 0) "" else t

  def Rep1Sep(t: TestGenerator, sep: String): TestGenerator = t ~ Rep1(sep ~ t)

  implicit def toTestGenerator(str: String): TestGenerator = Normal(str)

  implicit def mkTest(t: TestGenerator): String = t.toString

  val keywords = Set("Skip", "input", "if", "else", "while")

  def genId: String = {
    val length = nextInt(5)
    def capital = nextInt(2) * 32
    def alpha = (65 + nextInt(26) + capital).toChar
    val res = s"$alpha${(for (i <- 1 to length) yield {
      if (nextInt(2) == 0) (48 + nextInt(10)).toChar
      else alpha
    }).mkString("")}"
    if (keywords contains res) genId else res
  }

  def genTest(): Unit = {
    val testfile = new PrintWriter(new File("tests" ))
    for(i <- 0 to 1){
        val test = Prod("Commands").testWithDepth(3)
        testfile.write(test + "\n")
    }    
    testfile.close()
  }
  // PEG Grammar

  // TODO: Add more grammar
  val PEG_Grammar: Map[String, List[TestGenerator]] = Map(
    "Commands" -> List(
        Prod("Input") ~ ";" ~ Prod("Command"),
        Prod("Input") ~ ";" ~ Prod("Command") ~ Rep1(";" ~ Prod("Command"))
    ),
    "Command" -> List(
        Prod("Skip"),
        Prod("Input"),
        Prod("Substitute"),
        Prod("If"),
        Prod("While")
    ),
    "Skip" -> List(
        "Skip"
    ),
    "Input" -> List(
        "input(" ~ Prod("genId") ~ ")" 
    ),
    "Substitute" -> List(
        Prod("genId") ~ ":=" ~ Prod("Atom")
    ),
    "If" -> List(
        "if(" ~ Prod("BooleanExpression") ~ ")" ~ "{" ~ Prod("Commands") ~ "}" ~ "else{" ~ Prod("Commands") ~ "}"
    ),
    "While" -> List (
        "while(" ~ Prod("BooleanExpression") ~ ")" ~ "{" ~ Prod("Commands") ~ "}"
    ),
    "Expression" -> List(
        Prod("ScalarExpression"),
        Prod("BooleanExpression"),
    ),
    "ScalarExpression" -> List(
        Prod("Atom"),
        Prod("ScalarExpression") ~ "+" ~ Prod("ScalarExpression"),
        Prod("ScalarExpression") ~ "-" ~ Prod("ScalarExpression")
    ),
    "BooleanExpression" -> List(
        Prod("Boolean"),
        Prod("genId") ~ "==" ~ Prod("Num"),
        Prod("genId") ~ "<" ~ Prod("Num")
    ),
    "Atom" -> List(
        Prod("Num"),
        Prod("genId")
    ),
    "Num" -> (0 to 10).toList.map(x => {
        x.toString
    }),
    "Boolean" -> List(
        "true",
        "false"
    )
  )
}