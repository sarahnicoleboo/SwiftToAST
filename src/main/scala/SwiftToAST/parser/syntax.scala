package SwiftToAST.parser

import SwiftToAST.tokenizer._

case class Program(stmts: Seq[Stmt])
case class Variable(name: String)

sealed trait Op
case class Operator(value: String) extends Op

sealed trait TryModifier
case object NoTryModifier extends TryModifier
case object QuestionMarkTryModifier extends TryModifier
case object ExclamationTryModifier extends TryModifier


sealed trait InfixExp
case class WithOperatorInfixExpression(op: Operator, exp: Exp) extends InfixExp
case class TypeCastInfixExpression(op: TypeCastingOp) extends InfixExp
sealed trait TypeCastingOp
case class IsType(typ: Type) extends TypeCastingOp
case class AsType(typ: Type) extends TypeCastingOp
case class AsQuestionType(typ: Type) extends TypeCastingOp
case class AsBangType(typ: Type) extends TypeCastingOp

//exps
sealed trait Exp

case class TryExp(modifier: TryModifier, exp: Exp) extends Exp
case class PrefixExp(operator: Op, expression: Exp) extends Exp

case class PostfixExp(expression: Exp) extends Exp

case class NumericLiteralExp(value: String) extends Exp
case class SingleLineStringLiteralExp(value: String) extends Exp
case class MultiLineStringLiteralExp(value: String) extends Exp
case object TrueExp extends Exp
case object FalseExp extends Exp
case object NilExp extends Exp
case class ArrayLiteralExp(exps: List[Exp]) extends Exp
//case class DictionaryLiteralExp(exps: ???[Exp, Exp]) extends Exp

case class ColorPlaygroundLiteralExp(exp1: Exp, exp2: Exp, exp3: Exp, exp4: Exp) extends Exp
case class FilePlaygroundLiteralExp(exp: Exp) extends Exp
case class ImagePlaygroundLiteralExp(exp: Exp) extends Exp
case object HashFileExp extends Exp
case object HashFileIDExp extends Exp
case object HashFilePathExp extends Exp
case object HashLineExp extends Exp
case object HashColumnExp extends Exp
case object HashFunctionExp extends Exp
case object HashDSOHandleExp extends Exp


sealed trait Type




case class VariableExp(name: Variable) extends Exp


//statements
sealed trait Stmt

case class ExpressionStmt(exp: Exp) extends Stmt

case object TestStmt extends Stmt