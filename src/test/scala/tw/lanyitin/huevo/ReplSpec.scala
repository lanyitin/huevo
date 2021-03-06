package tw.lanyitin.huevo

import org.scalatest._
import machine.HObject

class ReplSpec extends FlatSpec with Matchers {
  "A REPL" should "able to do arithmatic calculation" in {
    val instructions: List[String] = """
      |2 * (3 * (3 + 1)) / 48.0
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getFloat == 0.5)
  }

  "A REPL" should "able to do boolean operation" in {
    val instructions: List[String] = """
      |true and true
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getBoolean == true)
  }

  "A REPL" should "able to do boolean operation2" in {
    val instructions: List[String] = """
      |true and false
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getBoolean == false)
  }

  "A REPL" should "able to do boolean operation3" in {
    val instructions: List[String] = """
      |false and false
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getBoolean == false)
  }

  "A REPL" should "able to do boolean operation4" in {
    val instructions: List[String] = """
      |false or true
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getBoolean == true)
  }

  "A REPL" should "able to do if expression" in {
    val instructions: List[String] = """
      |if (true) true else false
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getBoolean == true)
  }

  "A REPL" should "able to do if expression2" in {
    val instructions: List[String] = """
      |if (false) true else false
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getBoolean == false)
  }

  "A REPL" should "able to do function call" in {
    val instructions: String = """
      |def add(a: Integer, b: Integer): Integer = {a + b}
      |def add2(a: Integer, b: Integer): Integer = {a + add(a,b)}
      |def main(): Unit = add2(1,2)
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0).mkString("\n")
      Main.eval(instructions, true)
    assert(Main.top.getInt == 4)
  }

  "A REPL" should "able to do recursive function call" in {
    val instructions: String = """
      |def gcd(a: Integer, b: Integer): Integer = if (b != 0) {gcd(b, a%b)} else a
      |def main(): Unit = gcd(27,30)
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0).mkString("\n")
      Main.eval(instructions, true)
    assert(Main.top.getInt == 3)
  }
}