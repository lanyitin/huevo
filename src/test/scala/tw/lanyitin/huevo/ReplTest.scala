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
    println()
    val instructions: List[String] = """
      |if (true) true else false
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    assert(Main.top.getBoolean == true)
  }
}