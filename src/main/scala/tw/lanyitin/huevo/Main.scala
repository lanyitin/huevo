package tw.lanyitin.huevo

object Main {
  def main(args: Array[String]): Unit = {
    val content = "1 + 2 * 3 / 4 * (1 + 2) / (2 * 4) + ((1 * 3 / 3 - 1))"
    val scanner = Scanner(content)
    val result = Parser.parse(scanner)
    if (result.isSuccess) {
      println(result.get._1.visualize)
    }
  }
}