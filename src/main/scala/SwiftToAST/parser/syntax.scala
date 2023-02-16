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

case class GenericArgumentClause(typs: List[Type])

case class Attribute(name: IdentifierExp, argClause: List[BalancedToken])

sealed trait BalancedToken
case class InParensToken(token: BalancedToken) extends BalancedToken
case class InBracketsToken(token: BalancedToken) extends BalancedToken
case class InBracesToken(token: BalancedToken) extends BalancedToken
case class IdentifierToken(name: IdentifierExp) extends BalancedToken
//case class KeywordToken(name: ???) extends BalancedToken 
//case class 

//closure sig to be done
sealed trait ClosureSignature

sealed trait TupleElement
case class ExpTuple(exp: Exp) extends TupleElement
case class IdentifierColonExpTuple(identifierExp: IdentifierExp, exp: Exp) extends TupleElement

sealed trait DifferentIdentifiers
case class VariableExp(name: Variable) extends DifferentIdentifiers
case class ImplicitParameterExp(name: String) extends DifferentIdentifiers
case class PropertyWrapperProjectionExp(name: String) extends DifferentIdentifiers

sealed trait DifferentSelfs
case object SelfSolo extends DifferentSelfs
case class SelfMethod(exp: IdentifierExp) extends DifferentSelfs
case class SelfSubscript(functionCallArgList: List[FunctionCallArgument]) extends DifferentSelfs //questionable
case object SelfInit extends DifferentSelfs

sealed trait DifferentSuperClasses
case class SuperMethod(exp: IdentifierExp) extends DifferentSuperClasses
case class SuperSubscript(functionCallArgList: List[FunctionCallArgument]) extends DifferentSuperClasses
case object SuperInit extends DifferentSuperClasses

sealed trait FunctionCallArgument
case class ExpFunctionCallArgument(exp: Exp) extends FunctionCallArgument
case class IdentifierColonExpFunctionCallArgument(identifierExp: IdentifierExp, exp: Exp) extends FunctionCallArgument
case class OperatorFunctionCallArgument(op: Operator) extends FunctionCallArgument
case class IdentifierColonOperatorFunctionCallArgument(identifierExp: IdentifierExp, op: Operator) extends FunctionCallArgument

//exps
sealed trait Exp
case class TryExp(modifier: TryModifier, exp: Exp) extends Exp
case class PrefixExp(operator: Op, expression: Exp) extends Exp
case class PostfixExp(expression: Exp) extends Exp
case class CastExp(exp: Exp, op: TypeCastingOp) extends Exp
case class TrueInfixExp(exp1: Exp, op: Op, exp2: Exp) extends Exp
case class GenericVariableExp(exp: IdentifierExp, typs: GenericArgumentClause) extends Exp
case class IdentifierExp(exp: DifferentIdentifiers) extends Exp
/* case class VariableExp(name: Variable) extends Exp
case class ImplicitParameterExp(name: String) extends Exp
case class PropertyWrapperProjectionExp(name: String) extends Exp */
case class NumericLiteralExp(value: String) extends Exp
case class SingleLineStringLiteralExp(value: String) extends Exp
case class MultiLineStringLiteralExp(value: String) extends Exp
case object TrueExp extends Exp
case object FalseExp extends Exp
case object NilExp extends Exp
case class ArrayLiteralExp(exps: List[Exp]) extends Exp
case class DictionaryLiteralExp(exps: List[(Exp, Exp)]) extends Exp
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
case class SelfExp(exp: DifferentSelfs) extends Exp
case class SuperExp(exp: DifferentSuperClasses) extends Exp
case class ClosureExp(attributeList: List[Attribute], closureSigList: List[ClosureSignature], stmts: List[Stmt]) extends Exp
case class ParenthesizedExp(exp: Exp) extends Exp
case class TupleExp(elementList: List[TupleElement]) extends Exp

sealed trait Type
case class FunctionType(before: List[Type], after: Type) extends Type
case class ArrayType(typ: Type) extends Type
case class DictionaryType(type1: Type, type2: Type) extends Type
case class TypeIdentifier(typ: DifferentTypes) extends Type

sealed trait DifferentTypes
case class NormalType(typeName: IdentifierExp) extends DifferentTypes	//VariableExp to be more specific?
case class GenericType(typeName: IdentifierExp, genericTypes: GenericArgumentClause) extends DifferentTypes
case class NestedType(typeName: IdentifierExp, genericTypes: GenericArgumentClause, nestedType: TypeIdentifier) extends DifferentTypes

//statements
sealed trait Stmt
case class ExpressionStmt(exp: Exp) extends Stmt

case object TestStmt extends Stmt