package SwiftToAST.parser

case class Program(stmts: Seq[Stmt])
case class Variable(name: String)

sealed trait Exp

case class VariableExp(name: Variable) extends Stmt //change this back later!

sealed trait Type

case object IntType extends Type

//statements
sealed trait Stmt

case object TestStmt extends Stmt

//loop stmts
case class ForInStmt() extends Stmt	//come back bcuz need other shit