package tw.lanyitin.huevo

trait Expression

sealed trait Expression
case class Program(funDefs: List[FunctionDefinition]) extends Expression
case class FunctionDefinition(

def parse(scanner: Scanner): Program = {

}
