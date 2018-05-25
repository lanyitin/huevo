package tw.lanyitin.huevo.ast
import tw.lanyitin.huevo.Token
object NodeType extends Enumeration {
    type NodeType = Value
    val Expressions = Value("Expressions")
    val Expression = Value("Expression")
    val FunctionDefinition = Value("FunctionDefinition")
    val FunctionCall = Value("FunctionCall")
    val IfExpression = Value("IfExpression")
}


// TODO: add information of node type, or research on how to design AST
case class Node(token: Token, childs: List[Node]) {
  override def toString: String = {
    if (!childs.isEmpty) {
      s"Node($token,$childs)"
    } else {
      s"Node($token)"
    }
  }
    def gen_graphviz(): String = {
    val child_strs = childs.flatMap(x => f"${token.id} -> ${x.token.id}" :: x.gen_graphviz :: Nil)
    val node_label = token.id+ " [label=\"" + token.txt + s" ${token.line},${token.col}" + "\"]"
    (node_label :: child_strs).mkString("\n")
  }
}