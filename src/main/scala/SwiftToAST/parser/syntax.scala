package SwiftToAST.parser

import SwiftToAST.tokenizer._

case class Program(stmts: Seq[Stmt])
case class Variable(name: String)

sealed trait Op
case object DivisionOp extends Op
case object EqualOp extends Op
case object MinusOp extends Op
case object AdditionOp extends Op
case object NotOp extends Op
case object MultiplicationOp extends Op
case object ModOp extends Op
case object AndOp extends Op
case object OrOp extends Op
case object LessThanOp extends Op
case object GreaterThanOp extends Op
case object CaretOp extends Op
case object TildeOp extends Op
case object QuestionOp extends Op


case class TryOperator(token: Option[Token])	//change?

//exps
sealed trait Exp

case class SignedNumericLiteralExp(sign: Option[Token], value: NumericLiteralExp) extends Exp

sealed trait NumericLiteralExp
case class DecimalFloatLiteralExp(value: String) extends NumericLiteralExp
case class HexFloatLiteralExp(value: String) extends NumericLiteralExp
case class DecimalIntegerLiteralExp(value: String) extends NumericLiteralExp
case class BinaryIntegerLiteralExp(value: String) extends NumericLiteralExp
case class OctalIntegerLiteralExp(value: String) extends NumericLiteralExp
case class HexIntegerLiteralExp(value: String) extends NumericLiteralExp

case class VariableExp(name: Variable) extends Exp

//types
sealed trait Type

case object IntType extends Type

//statements
sealed trait Stmt

case object TestStmt extends Stmt