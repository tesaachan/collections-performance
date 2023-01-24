import scala.util.Random
import scala.io.StdIn
import io.AnsiColor._


object Perf {
  val immutSeq = Vector(
    "List"         -> "ccllcl-",
    "LazyList"     -> "ccllcl-",
    "ArraySeq (I)" -> "clclll-",
    "Vector"       -> ("ec"*6 ++ "-"),
    "Queue (I)"    -> "acaclllc-",
    "Range"        -> ("ccc" ++ "-"*4),
    "String"       -> "clclll-",
  )

  val immutSetAndMap = Vector(
    "HashSet/HashMap" -> ("ec"*3 ++ "l"),
    "TreeSet/TreeMap" -> ("log"*4),
    "BitSet (I)"      -> "cllcec",
    "VectorMap"       -> "ececacl",
    "ListMap"         -> "llll",
  )

  val mutSeq = Vector(
    "ArrayBuffer"   -> "clcclacl",
    "ListBuffer"    -> "clllccl",
    "StringBuilder" -> "clcclacl",
    "Queue (M)"     -> "clllccl",
    "ArraySeq (M)"  -> "clcc---",
    "Stack"         -> "clllcll",
    "Array"         -> "clcc---",
    "ArrayDeque"    -> "clccacacl",
  )

  val mutSetAndMap = Vector(
    "HashSet/HashMap" -> ("ec"*3 + "l"),
    "WeakHashMap"     -> ("ec"*3 + "l"),
    "BitSet"          -> "caccec",
    "TreeSet"         -> ("log"*4)
  )

  val allImmutable = immutSeq ++ immutSetAndMap

  val allMutable = mutSeq ++ mutSetAndMap

  val all = allImmutable ++ allMutable
}

object ScriptRunner {
  def getCollection(v: Vector[(String, String)]): (String, String) =
    v(Random.nextInt(v.size))

  def iteration(rightAnswers: Int, allAnswers: Int, colvec: Vector[(String, String)]): Unit = {
    val (colName, colAnswer) = getCollection(colvec)
    println(s"Collection: $colName\nYour answer:${YELLOW}")

    def userAnswer(): String = {
      val answer = StdIn.readLine().split(" ").mkString.toLowerCase()
      print(s"${RESET}")
      answer
    }

    def showAnswersRate(): Unit = {
      val rate = ((rightAnswers.toDouble / allAnswers) * 100).toInt
      val message = if (allAnswers == 0) "0" else rate match {
        case good if good > 65 =>
          s"${GREEN}${good}${RESET}"
        case mid if mid > 49 =>
          s"${YELLOW}${mid}${RESET}"
        case bad =>
          s"${RED}${bad}${RESET}"
      }
      println(s"Right answers during this session: $message % ($rightAnswers/$allAnswers)")
    }
    
    def checkAnswer: String => Unit = {
      case "exit"      => ()
      case "back"      => begin()
      case "stat"      => 
        showAnswersRate()
        checkAnswer(userAnswer())
      case "info"      => 
        println("\neC - effectively constant time.\nDepends on some asumptions.\n" ++
                "Such as max length of a vector or distribution of hash keys.\n" ++
                "aC - amortized constant time.\nSome invocations of the operation takes longer.\n" ++
                "But for a big amount of operations on average only constant time per operation is taken.\n")
        checkAnswer(userAnswer())
      case `colAnswer` => 
        println(s"${GREEN}^_^ Congratulations!\n${RESET}")
        iteration(rightAnswers + 1, allAnswers + 1, colvec)
      case _           => 
        println(s"${RED}>_< No(((\n${RESET}")
        iteration(rightAnswers, allAnswers + 1, colvec)
    }
    checkAnswer(userAnswer())
  }

  def begin(): Unit = {
    println("Type 1 for immutable, 2 for mutable, 3 for all, exit to leave:")
    print(s"${YELLOW}")
    val input = StdIn.readLine().toLowerCase() 
    print(s"${RESET}")
    input match {
      case "exit"  => ()
      case str if str
        .toIntOption
        .filter((1 to 3).contains(_))
        .isDefined => iteration(0, 0, str match {
          case "1" => Perf.allImmutable
          case "2" => Perf.allMutable
          case "3" => Perf.all
        })
      case _       => begin()
    }
  }

}

object Main extends App {
  println("Welcome to collection performance check!!!")
  println("----------------------------------------------------------------")
  println("For Seq type:      head tail apply update prepend append insert")
  println("For Set/Map type:  lookup add remove min")
  println("----------------------------------------------------------------")
  ScriptRunner.begin()
}
