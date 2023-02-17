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
case class InParensBalancedToken(token: BalancedToken) extends BalancedToken
case class InBracketsBalancedToken(token: BalancedToken) extends BalancedToken
case class InBracesBalancedToken(token: BalancedToken) extends BalancedToken
case class IdentifierBalancedToken(name: IdentifierExp) extends BalancedToken
case class KeywordBalancedToken(name: Keyword) extends BalancedToken
case class LiteralBalancedToken(name: Exp) extends BalancedToken
case class OperatorBalancedToken(op: Operator) extends BalancedToken
case class PunctuationBalancedToken(punc: Punctuation) extends BalancedToken
 
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

sealed trait DifferentImplicitMembers
case class IdentifierImplicitMember(identifierExp: IdentifierExp) extends DifferentImplicitMembers
case class IdentifierDotPostFixMember(identifierExp: IdentifierExp, postfixExp: Exp) extends DifferentImplicitMembers

//exps
sealed trait Exp
case class TryExp(modifier: TryModifier, exp: Exp) extends Exp
case class PrefixExp(operator: Op, expression: Exp) extends Exp
case class PostfixExp(expression: Exp) extends Exp
case class CastExp(exp: Exp, op: TypeCastingOp) extends Exp
case class TrueInfixExp(exp1: Exp, op: Op, exp2: Exp) extends Exp
case class GenericVariableExp(exp: IdentifierExp, typs: GenericArgumentClause) extends Exp
case class IdentifierExp(exp: DifferentIdentifiers) extends Exp
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
case class ImplicitMemberExp(exps: DifferentImplicitMembers) extends Exp
case object WildcardExp extends Exp

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

//punctuation
sealed trait Punctuation
case object Period extends Punctuation
case object Comma extends Punctuation
case object Colon extends Punctuation
case object Semicolon extends Punctuation
case object Equal extends Punctuation
case object At extends Punctuation
case object Hash extends Punctuation
case object BackTick extends Punctuation
case object Question extends Punctuation
case object Arrow extends Punctuation
case object And extends Punctuation
case object Exclamation extends Punctuation

//keywords
sealed trait Keyword
//"used in declarations"
case object AssociatedTypeKeyword extends Keyword
case object ClassKeyword extends Keyword
case object DeinitKeyword extends Keyword
case object EnumKeyword extends Keyword
case object ExtensionKeyword extends Keyword
case object FilePrivateKeyword extends Keyword
case object FuncKeyword extends Keyword
case object ImportKeyword extends Keyword
case object InitKeyword extends Keyword
case object InOutKeyword extends Keyword
case object InternalKeyword extends Keyword
case object LetKeyword extends Keyword
case object OpenKeyword extends Keyword
case object OperatorKeyword extends Keyword
case object PrivateKeyword extends Keyword
case object ProtocolKeyword extends Keyword
case object PublicKeyword extends Keyword
case object RethrowsKeyword extends Keyword
case object StaticKeyword extends Keyword
case object StructKeyword extends Keyword
case object SubscriptKeyword extends Keyword
case object TypeAliasKeyword extends Keyword
case object VarKeyword extends Keyword
//"used in statements"
case object BreakKeyword extends Keyword
case object ContinueKeyword extends Keyword
case object DefaultKeyword extends Keyword
case object DeferKeyword extends Keyword
case object DoKeyword extends Keyword
case object ElseKeyword extends Keyword
case object FallthroughKeyword extends Keyword
case object ForKeyword extends Keyword
case object GuardKeyword extends Keyword
case object IfKeyword extends Keyword
case object InKeyword extends Keyword
case object RepeatKeyword extends Keyword
case object ReturnKeyword extends Keyword
case object SwitchKeyword extends Keyword
case object WhereKeyword extends Keyword
case object WhileKeyword extends Keyword
//"used in expressions and types"
case object AsKeyword extends Keyword
case object AnyKeyword extends Keyword
case object CatchKeyword extends Keyword
case object FalseKeyword extends Keyword
case object IsKeyword extends Keyword
case object NilKeyword extends Keyword
case object SuperKeyword extends Keyword
case object SelfKeyword extends Keyword
case object SelfBigKeyword extends Keyword
case object ThrowKeyword extends Keyword
case object ThrowsKeyword extends Keyword
case object TrueKeyword extends Keyword
case object TryKeyword extends Keyword
//"used in patterns"
case object UnderscoreKeyword extends Keyword
//"begin with a #"
case object AvailableKeyword extends Keyword
case object HashColorLiteralKeyword extends Keyword
case object HashColumnKeyword extends Keyword //also appears in Exp
case object HashElseKeyword extends Keyword
case object HashElseIfKeyword extends Keyword
case object HashEndIfKeyword extends Keyword
case object ErrorKeyword extends Keyword
case object HashFileKeyword extends Keyword //also appears in Exp
case object HashFileIDKeyword extends Keyword //also appears in Exp
case object HashFileLiteralKeyword extends Keyword
case object HashFilePathKeyword extends Keyword //also appears in Exp
case object HashFunctionKeyword extends Keyword //also appears in Exp
case object HashIfKeyword extends Keyword
case object HashImageLiteralKeyword extends Keyword //also appears in Exp
case object HashLineKeyword extends Keyword //also appears in Exp
case object HashSelectorKeyword extends Keyword
case object SourceLocationKeyword extends Keyword
case object WarningKeyword extends Keyword