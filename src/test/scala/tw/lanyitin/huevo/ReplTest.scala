package tw.lanyitin.huevo

import org.scalatest._

class ReplSpec extends FlatSpec with Matchers {
  "A REPL" should "able to do arithmatic calculation and boolean operation" in {
    val instructions: List[String] = """
      |2 * (3 * 4) / 48.0
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    instructions.foreach(inst => {
      Main.eval(inst)
    })
    println(Main.top)
  }
}