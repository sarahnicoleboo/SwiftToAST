package SwiftToAST.parser

import SwiftToAST.tokenizer._

case class Program(stmts: Seq[Stmt])
case class Variable(name: String)

//ops
sealed trait Op
case class Operator(value: String) extends Op


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
case class ClosureExp(attributeList: Option[List[Attribute]], closureSig: Option[ClosureSignature], stmts: Option[List[Stmt]]) extends Exp
case class ParenthesizedExp(exp: Exp) extends Exp
case class TupleExp(elementList: List[TupleElement]) extends Exp
case class ImplicitMemberExp(exps: DifferentImplicitMembers) extends Exp
case object WildcardExp extends Exp
case class KeyPathExp(typ: Option[Type], keyPathComponents: List[KeyPathComponent]) extends Exp
//
case class AssignmentExp(id: IdentifierExp, exp: Exp) extends Exp
//

//helpers for exps
//also hacky but works i guess
sealed trait InOutMod
case object InOutModifier extends InOutMod
//above are also used as helpers for types

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

sealed trait ClosureSignature
case class ClosureSignatureComplex(list: Option[CaptureList], clause: ClosureParameterClause, asynch: Option[AsyncMod], throws: Option[ThrowsMod], functionResult: Option[FunctionResult]) extends ClosureSignature
case class ClosureSignatureSimple(list: CaptureList) extends ClosureSignature

//hacky but works i guess
sealed trait AsyncMod
case object AsyncModifier extends AsyncMod
sealed trait ThrowsMod
case object ThrowsModifier extends ThrowsMod

case class CaptureList(list: List[CaptureListItem])
sealed trait CaptureListItem
case class CaptureListItemIdentifier(specifier: Option[CaptureSpecifier], id: IdentifierExp) extends CaptureListItem
case class CaptureListItemAssignment(specifier: Option[CaptureSpecifier], assignmentExp: AssignmentExp) extends CaptureListItem
case class CaptureListItemSelf(specifier: Option[CaptureSpecifier], self: SelfExp) extends CaptureListItem

sealed trait CaptureSpecifier
case object WeakCaptureSpecifier extends CaptureSpecifier
case object UnownedCaptureSpecifier extends CaptureSpecifier
case object UnownedSafeCaptureSpecifier extends CaptureSpecifier
case object UnownedUnsafeCaptureSpecifier extends CaptureSpecifier

sealed trait ClosureParameterClause
case class CPCIdentifierList(list: List[IdentifierExp]) extends ClosureParameterClause
case class CPCClosureParameterList(list: List[ClosureParameter]) extends ClosureParameterClause

sealed trait ClosureParameter
case class ClosureParameterReg(id: IdentifierExp, typeAnnotation: Option[TypeAnnotation]) extends ClosureParameter
case class ClosureParameterElipses(id: IdentifierExp, typeAnnotation: TypeAnnotation) extends ClosureParameter

case class FunctionResult(attributes: List[Attribute], typ: Type)

sealed trait TupleElement
case class ExpTuple(exp: Exp) extends TupleElement
case class IdentifierColonExpTuple(identifierExp: IdentifierExp, exp: Exp) extends TupleElement

sealed trait DifferentIdentifiers
case class VariableExp(name: Variable) extends DifferentIdentifiers
case class ImplicitParameterExpOrPWP(name: String) extends DifferentIdentifiers
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

sealed trait KeyPathComponent
case class IdentifierThenOptPostfixesKPC(id: IdentifierExp, postfixes: Option[List[KeyPathPostfix]]) extends KeyPathComponent
case class PostfixesKPC(postfixes: List[KeyPathPostfix]) extends KeyPathComponent

sealed trait KeyPathPostfix
case object QuestionKPP extends KeyPathPostfix
case object ExclamationKPP extends KeyPathPostfix
case object SelfKPP extends KeyPathPostfix
case class FuncCallArgListKPP(list: List[FunctionCallArgument]) extends KeyPathPostfix

//types
case class TypeAnnotation(attributes: Option[List[Attribute]], inout: Option[InOutMod], typ: Type)

sealed trait Type
//WRONG
//case class FinalFormType(first: Type, second: TrailorType) extends Type
case class FunctionType(optAttributes: Option[List[Attribute]], argClause: FunctionTypeArgClause, async: Option[AsyncMod], throws: Option[ThrowsMod], typ: Type) extends Type
case class ArrayType(typ: Type) extends Type
case class DictionaryType(type1: Type, type2: Type) extends Type
case class TypeIdentifier(typ: DifferentTypes) extends Type
case class TupleType(list: List[TupleTypeElement]) extends Type
case class ProtocolCompositionType(typeIDs: List[TypeIdentifier]) extends Type
case class OpaqueType(typ: Type) extends Type
case object AnyType extends Type
case object SelfType extends Type
case class InParensType(typ: Type) extends Type
case class OptionalType(typ: Type) extends Type
case class ImplicitlyUnwrappedOptionalType(typ: Type) extends Type

sealed trait TrailorTypeThing
case object OptionalTypeThing extends TrailorTypeThing
case object ImplicitlyUnwrappedOptionalTypeThing extends TrailorTypeThing

//helpers for types
case class FunctionTypeArgClause(list: List[FunctionTypeArg])

sealed trait FunctionTypeArg
//i am confused because the below is exactly the same as a type annotation
case class FunctionTypeArg1(attributesList: Option[List[Attribute]], inout: Option[InOutMod], typ: Type) extends FunctionTypeArg
case class FunctionTypeArg2(id: IdentifierExp, typeAnnotation: TypeAnnotation) extends FunctionTypeArg

sealed trait DifferentTypes
case class NormalType(typeName: IdentifierExp) extends DifferentTypes
case class GenericType(typeName: IdentifierExp, genericTypes: GenericArgumentClause) extends DifferentTypes
case class NestedNormalType(typeName: IdentifierExp, nestedType: TypeIdentifier) extends DifferentTypes
case class NestedGenericType(typeName: IdentifierExp, genericTypes: GenericArgumentClause, nestedType: TypeIdentifier) extends DifferentTypes

sealed trait TupleTypeElement
case class TupleTypeElementNameAnnotation(id: IdentifierExp, typeAnnotation: TypeAnnotation) extends TupleTypeElement
case class TupleTypeElementType(typ: Type) extends TupleTypeElement

//statements
sealed trait Stmt
case class ExpressionStmt(exp: Exp) extends Stmt

case object TestStmt extends Stmt

//random shit down here
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