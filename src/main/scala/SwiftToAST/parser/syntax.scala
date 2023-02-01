package SwiftToAST.parser

sealed trait Exp



//statements
sealed trait Stmt

//loop stmts
case class ForInStmt() extends Stmt	//come back bcuz need other shit