package tw.lanyitin.huevo

object Main {
  def main(args: Array[String]): Unit = {
    val content = "1 + 2 * 3 / 4 * (1 + 2) / (2 * 4) + ((1 * 3 / 3 - 1))"
    val scanner = Scanner(content, (x:String) => ())
    val (tree, _) = Parser.parse(scanner)
    println(tree.gen_graphviz)
  }
}