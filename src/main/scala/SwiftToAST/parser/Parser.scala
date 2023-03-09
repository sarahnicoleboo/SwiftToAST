package SwiftToAST.parser

import SwiftToAST.tokenizer._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class ParserException(message: String) extends Exception(message)

object Parser extends Parsers {
	override type Elem = Token
	
	lazy val variable: Parser[VariableToken] = {
		accept("variable", { case id @ VariableToken(name) => id })
	}
	
/* 	lazy val implicit_parameter: Parser[ImplicitParameterToken] = {
		accept("implicit_parameter", { case id @ ImplicitParameterToken(name) => id })
	} */
	
	lazy val property_wrapper_projection: Parser[PropertyWrapperProjectionToken] = {
		accept("property_wrapper_projection", { case id @ PropertyWrapperProjectionToken(name) => id })
	}
	
	lazy val ip_OR_pwp: Parser[ImplicitParameterOrPropertyWrapperProjectionToken] = {
		accept("ip_OR_pwp", { case id @ ImplicitParameterOrPropertyWrapperProjectionToken(name) => id })
	}
	
	lazy val binary_integer: Parser[BinaryIntegerLiteralToken] = {
		accept("binary_integer", { case id @ BinaryIntegerLiteralToken(value) => id })
	}
	
	lazy val octal_integer: Parser[OctalIntegerLiteralToken] = {
		accept("octal_integer", { case id @ OctalIntegerLiteralToken(value) => id })
	}
	
	lazy val decimal_integer: Parser[DecimalIntegerLiteralToken] = {
		accept("decimal_integer", { case id @ DecimalIntegerLiteralToken(value) => id })
	}
	
	lazy val hex_integer: Parser[HexIntegerLiteralToken] = {
		accept("hex_integer", { case id @ HexIntegerLiteralToken(value) => id })
	}
	
	lazy val decimal_float: Parser[FloatDecimalLiteralToken] = {
		accept("decimal_float", { case id @ FloatDecimalLiteralToken(value) => id })
	}
	
	lazy val hex_float: Parser[FloatHexLiteralToken] = {
		accept("hex_float", { case id @ FloatHexLiteralToken(value) => id })
	}
	
	lazy val single_line_string: Parser[SingleLineStringLiteralToken] = {
		accept("single_line_string", { case id @ SingleLineStringLiteralToken(value) => id })
	}
	
	lazy val multi_line_string: Parser[MultiLineStringLiteralToken] = {
		accept("multi_line_string", { case id @ MultiLineStringLiteralToken(value) => id })
	}
	
	lazy val operator_thing: Parser[OperatorLiteralToken] = {
		accept("operator_thing", { case id @ OperatorLiteralToken(value) => id })
	}
	
	lazy val dot_operator_thing: Parser[DotOperatorLiteralToken] = {
		accept("dot_operator_thing", { case id @ DotOperatorLiteralToken(value) => id })
	}
	
	def operator(expected: String): Parser[Operator] = {
		operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure("dfsd"))
	}
	
	def dot_operator(expected: String): Parser[Operator] = {
		dot_operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure("oops"))
	}
		
	class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
	override def first: Token = tokens.head
	override def atEnd: Boolean = tokens.isEmpty
	override def pos: Position = NoPosition
	override def rest: Reader[Token] = new TokenReader(tokens.tail)
	}
	
/* 	def apply(tokens: Seq[Token]): Program = {
		val reader = new TokenReader(tokens)
		program(reader) match {
			case NoSuccess(message, next) => throw new ParserException(message)
			case Success(result, next) => result
		}
	} */
	
	def apply[A](parser: Parser[A], tokens: Seq[Token]): A = {
		val reader = new TokenReader(tokens)
		parser(reader) match {
			case NoSuccess(message, next) => throw new ParserException(message)
			case Success(result, next) => result
		}
	}
	
	def program: Parser[Program] = phrase(block)
	
	def block: Parser[Program] = {
		rep(statement) ^^ { case stmts => Program(stmts)}
	}
	
	//statements
	lazy val statements: Parser[List[Stmt]] = {
		rep1(statement)
	}
	
	lazy val statement: Parser[Stmt] = {
		expression_stmt
	}
	
	lazy val expression_stmt: Parser[Stmt] = {
		expression ~ opt(SemicolonToken) ^^ { case exp ~ _ => ExpressionStmt(exp) }
	}
	
	lazy val expression: Parser[Exp] = {
		opt(try_operator) ~ prefix_expression ~ opt(infix_expression) ^^ {case theTry ~ prefix ~ infix => {
			val combinedExp = infix match {
				case None => prefix
				case Some(WithOperatorInfixExpression(op, exp)) => TrueInfixExp(prefix, op, exp)//1 + 2 as single expression
				case Some(TypeCastInfixExpression(op)) => CastExp(prefix, op)
			}
			theTry match {
				case None => combinedExp
				case Some(tryModifier) => TryExp(tryModifier, combinedExp)
			}
		}}
	}

	lazy val prefix_expression: Parser[Exp] = {
		//opt(prefix_operator) ~ postfix_expression ^^ { case optOperator ~ expression => optOperator.map(_ => PrefixExp(optOperator, expression)).getOrElse(expression) }
		prefix_operator ~ postfix_expression ^^ { case theOperator ~ exp => PrefixExp(theOperator, exp) } |
		postfix_expression ^^ { case exp => PostfixExp(exp) }
	}
	
	lazy val in_out_expression: Parser[Exp] = {
		???
	}
	
	lazy val infix_expression: Parser[InfixExp] = {
	infix_operator ~ prefix_expression ^^ { case op ~ exp => WithOperatorInfixExpression(op, exp) }
	//still need type casting operator but i need to do types for that so hold up
	}
	

	lazy val postfix_expression: Parser[Exp] = {
		primary_expression
	}
	
	lazy val primary_expression: Parser[Exp] = {
		identifier ~ generic_argument_clause ^^ { case varName ~ listOfTypes => GenericVariableExp(varName, listOfTypes) } |
		identifier |
		literal_expression |
		self_expression |
		superclass_expression |
		closure_expression |	//stopped testing here
		parenthesized_expression |
		tuple_expression |
		implicit_member_expression |
		wildcard_expression |
		key_path_expression
		//still need more
	}
	
	lazy val identifier: Parser[IdentifierExp] = {
		variable ^^ { case VariableToken(name) => IdentifierExp(VariableExp(Variable(name))) } |
		ip_OR_pwp ^^ { case ImplicitParameterOrPropertyWrapperProjectionToken(name) => IdentifierExp(ImplicitParameterExpOrPWP(name)) } |
		property_wrapper_projection ^^ { case PropertyWrapperProjectionToken(name) => IdentifierExp(PropertyWrapperProjectionExp(name)) }
	}
	
	lazy val generic_argument_clause: Parser[GenericArgumentClause] = {
		operator("<") ~ comma_sep_types ~ operator(">") ^^ { case _ ~ typs ~ _ => GenericArgumentClause(typs) }
	}
	
	lazy val comma_sep_types: Parser[List[Type]] = {
		rep1sep (typ, CommaToken)
	}
	

	lazy val literal_expression: Parser[Exp] = {
		literal | array_literal |
		dictionary_literal | playground_literal |
		HashFileToken ^^^ HashFileExp |
		HashFileIDToken ^^^ HashFileIDExp |
		HashFilePathToken ^^^ HashFilePathExp |
		HashLineToken ^^^ HashLineExp |
		HashColumnToken ^^^ HashColumnExp |
		HashFunctionToken ^^^ HashFunctionExp |
		HashDSOHandleToken ^^^ HashDSOHandleExp
	}
	
	lazy val literal: Parser[Exp] = {
		numeric_literal | string_literal |
		boolean_literal | nil_literal
	}
	
	lazy val numeric_literal: Parser[Exp] = {
		opt(operator("-")) ~ integer_literal ^^ { case optMinus ~ intLiteral => optMinus.map(_ => PrefixExp(Operator("-"), intLiteral)).getOrElse(intLiteral) } |
		opt(operator("-")) ~ float_literal ^^ { case optMinus ~ floatLiteral => optMinus.map(_ => PrefixExp(Operator("-"), floatLiteral)).getOrElse(floatLiteral) }
	}
	
	lazy val integer_literal: Parser[NumericLiteralExp] = {
		decimal_integer ^^ { case DecimalIntegerLiteralToken(value) => NumericLiteralExp(value) } |
		binary_integer ^^ { case BinaryIntegerLiteralToken(value) => NumericLiteralExp(value) } |
		octal_integer ^^ { case OctalIntegerLiteralToken(value) => NumericLiteralExp(value) } |
		hex_integer ^^ { case HexIntegerLiteralToken(value) => NumericLiteralExp(value) }
	}
	
	lazy val float_literal: Parser[NumericLiteralExp] = {
		decimal_float ^^ { case FloatDecimalLiteralToken(value) => NumericLiteralExp(value) } |
		hex_float ^^ { case FloatHexLiteralToken(value) => NumericLiteralExp(value) }
	}
	
	lazy val string_literal: Parser[Exp] = {
		single_line_string ^^ { case SingleLineStringLiteralToken(value) => SingleLineStringLiteralExp(value) } |	//interpolation(value)*/ }
		multi_line_string ^^ { case MultiLineStringLiteralToken(value) => MultiLineStringLiteralExp(value) }
	}
	
	lazy val boolean_literal: Parser[Exp] = {
		TrueToken ^^^ TrueExp | FalseToken ^^^ FalseExp
	}
	
	lazy val nil_literal: Parser[Exp] = {
		NilToken ^^^ NilExp
	}
	
	lazy val array_literal: Parser[Exp] = {
		(LeftBracketToken ~ comma_sep_exps ~ opt(CommaToken) ~ RightBracketToken).flatMap(
			{ case _ ~ expList ~ maybe ~ _ =>
				if(expList.isEmpty && maybe.nonEmpty)
					{ failure("random comma not preceeded by an exp") } else { success(ArrayLiteralExp(expList)) }})
	}
	
	lazy val comma_sep_exps: Parser[List[Exp]] = {
		repsep(expression, CommaToken)
	}
	
	lazy val dictionary_literal: Parser[Exp] = {
		LeftBracketToken ~ ColonToken ~ RightBracketToken ^^^ DictionaryLiteralExp(List()) |
		LeftBracketToken ~ comma_sep_dictionary ~ opt(CommaToken) ~ RightBracketToken ^^ { case _ ~ list ~ _ ~ _ => DictionaryLiteralExp(list) } //|
	}
	
	lazy val comma_sep_dictionary: Parser[List[(Exp, Exp)]] = {
		rep1sep(expression ~ ColonToken ~ expression, CommaToken).map(_.map(input => (input._1._1, input._2)))
	}
	
	lazy val playground_literal: Parser[Exp] = {
		HashColorLiteralToken ~ LeftParenToken ~ RedToken ~ ColonToken ~ expression ~ CommaToken ~ GreenToken ~ ColonToken ~ expression ~ CommaToken ~ BlueToken ~ ColonToken ~ expression ~ CommaToken ~ AlphaToken ~ ColonToken ~ expression ~ RightParenToken ^^ 
			{ case _ ~ _ ~ _ ~ _ ~ exp1 ~ _ ~ _ ~ _ ~ exp2 ~ _ ~ _ ~ _ ~ exp3 ~ _ ~ _ ~ _ ~ exp4 ~ _ => ColorPlaygroundLiteralExp(exp1, exp2, exp3, exp4) } |
		HashFileLiteralToken ~ LeftParenToken ~ ResourceNameToken ~ ColonToken ~ expression ~ RightParenToken ^^
			{ case _ ~ _ ~ _ ~ _ ~ exp ~ _ =>  FilePlaygroundLiteralExp(exp) } |
		HashImageLiteralToken ~ LeftParenToken ~ ResourceNameToken ~ ColonToken ~ expression ~ RightParenToken ^^
			{ case _ ~ _ ~ _ ~ _ ~ exp ~ _ => ImagePlaygroundLiteralExp(exp) }
	}
	
	lazy val self_expression: Parser[SelfExp] = {
		SelfToken ~ dot_operator(".") ~ identifier ^^ { case _ ~ _ ~ identifierExp => SelfExp(SelfMethod(identifierExp)) } |
		SelfToken ~ dot_operator(".") ~ InitToken ^^^ SelfExp(SelfInit) |
		SelfToken ~ LeftBracketToken ~ function_call_argument_list ~ RightBracketToken ^^ { case _ ~ _ ~ list ~ _ => SelfExp(SelfSubscript(list)) } |
		SelfToken ^^^ SelfExp(SelfSolo)
	}
	
	lazy val superclass_expression: Parser[SuperExp] = {
		SuperToken ~ dot_operator(".") ~ identifier ^^ { case _ ~ _ ~ identifierExp => SuperExp(SuperMethod(identifierExp)) } |
		SuperToken ~ dot_operator(".") ~ InitToken ^^^ SuperExp(SuperInit) |
		SuperToken ~ LeftBracketToken ~ function_call_argument_list ~ RightBracketToken ^^ { case _ ~ _ ~ list ~ _ => SuperExp(SuperSubscript(list)) }
	}
	
	lazy val function_call_argument_list: Parser[List[FunctionCallArgument]] = {
		rep1sep(function_call_argument, CommaToken)
	}
	
	lazy val function_call_argument: Parser[FunctionCallArgument] = {
		identifier ~ ColonToken ~ expression ^^ { case id ~ _ ~ exp => IdentifierColonExpFunctionCallArgument(id, exp) } |
		identifier ~ ColonToken ~ operator ^^ { case id ~ _ ~ op => IdentifierColonOperatorFunctionCallArgument(id, op) } |
		expression ^^ { case exp => ExpFunctionCallArgument(exp) } |
		operator ^^ { case op => OperatorFunctionCallArgument(op) }
	}
	
	lazy val closure_expression: Parser[ClosureExp] = {
		LeftCurlyToken ~ opt(attributes) ~ opt(closure_signature) ~ opt(statements) ~ RightCurlyToken ^^ { case _ ~ optAttributes ~ optSig ~ optStmts ~ _ => ClosureExp(optAttributes, optSig, optStmts) }
	}
	
	//attributes
	lazy val attributes: Parser[List[Attribute]] = {
		rep1(attribute)
	}
	
	lazy val attribute: Parser[Attribute] = {
		//below is commented out because we are CHEATING
		//AtToken ~ identifier ~ attribute_argument_clause ^^ { case _ ~ name ~ clause => Attribute(name, clause) } |
		AtToken ~ identifier ^^ { case _ ~ name => Attribute(name, List()) }
	}
	
	lazy val attribute_argument_clause: Parser[List[BalancedToken]] = {
		LeftParenToken ~ rep(balanced_token) ~ RightParenToken ^^ { case _ ~ tokens ~ _ => tokens}
	}
	
	lazy val balanced_token: Parser[BalancedToken] = {
		LeftParenToken ~ balanced_token ~ RightParenToken ^^ { case _ ~ token ~ _ => InParensBalancedToken(token) } |
		LeftBracketToken ~ balanced_token ~ RightBracketToken ^^ { case _ ~ token ~ _ => InBracketsBalancedToken(token) } |
		LeftCurlyToken ~ balanced_token ~ RightCurlyToken ^^ { case _ ~ token ~ _ => InBracesBalancedToken(token) } |
		identifier ^^ { case id => IdentifierBalancedToken(id) } |
		keyword ^^ { case word => KeywordBalancedToken(word) } |	//bottom of file bcuz big af
		literal ^^ { case lit => LiteralBalancedToken(lit) } |
		punctuation ^^ { case punc => PunctuationBalancedToken(punc) } |	//also bottom of file with keywords
		operator ^^ { case op => OperatorBalancedToken(op) }
	}
	
	lazy val closure_signature: Parser[ClosureSignature] = {
		opt(capture_list) ~ closure_parameter_clause ~ opt(asynch_modifier) ~ opt(throws_modifier) ~ opt(function_result) ~ InToken ^^
			{ case captureList ~ clause ~ asynch ~ throws ~ funcResult ~ _ => ClosureSignatureComplex(captureList, clause, asynch, throws, funcResult) } |
		capture_list ~ InToken ^^ { case list ~ _ => ClosureSignatureSimple(list) } 
	}
	
	lazy val capture_list: Parser[CaptureList] = {
		LeftBracketToken ~ rep1sep(capture_list_item, CommaToken) ~ RightBracketToken ^^ { case _ ~ list ~ _ => CaptureList(list) }
	}
	
	lazy val capture_list_item: Parser[CaptureListItem] = {
		opt(capture_specifier) ~ assignment_exp ^^ { case cp ~ exp => CaptureListItemAssignment(cp, exp) } |
		opt(capture_specifier) ~ identifier ^^ { case cp ~ id => CaptureListItemIdentifier(cp, id) } |
		//opt(capture_specifier) ~ assignment_exp ^^ { case cp ~ exp => CaptureListItemAssignment(cp, exp) } |
		opt(capture_specifier) ~ self_expression ^^ { case cp ~ self => CaptureListItemSelf(cp, self) }
	}
	
	lazy val capture_specifier: Parser[CaptureSpecifier] = {
		WeakToken ^^^ WeakCaptureSpecifier |
		UnownedToken ~ LeftParenToken ~ SafeToken ~ RightParenToken ^^^ UnownedSafeCaptureSpecifier |
		UnownedToken ~ LeftParenToken ~ UnsafeToken ~ RightParenToken ^^^ UnownedUnsafeCaptureSpecifier |
		UnownedToken ^^^ UnownedCaptureSpecifier
	}
	
	lazy val closure_parameter_clause: Parser[ClosureParameterClause] = {
		rep1sep(identifier, CommaToken) ^^ { case idList => CPCIdentifierList(idList) } |
		LeftParenToken ~ rep1sep(closure_parameter, CommaToken) ~ RightParenToken ^^ { case _ ~ paramList ~ _ => CPCClosureParameterList(paramList) } |
		LeftParenToken ~ RightParenToken ^^^ CPCClosureParameterList(List())
	}
	
	lazy val closure_parameter: Parser[ClosureParameter] = {
		identifier ~ type_annotation ~ operator("...") ^^ { case id ~ annotation ~ _ => ClosureParameterElipses(id, annotation) } |
		identifier ~ opt(type_annotation) ^^ { case id ~ optAnnotation => ClosureParameterReg(id, optAnnotation) }
	}
	
	lazy val asynch_modifier: Parser[AsyncMod] = { AsyncToken ^^^ AsyncModifier }
	
	lazy val throws_modifier: Parser[ThrowsMod] = { ThrowsToken ^^^ ThrowsModifier }
	
	lazy val function_result: Parser[FunctionResult] = {
		rep(attribute) ~ typ ^^ { case optAttributes ~ theType => FunctionResult(optAttributes, theType) }
	}
	
	lazy val assignment_exp: Parser[AssignmentExp] = {
		identifier ~ operator("=") ~ expression ^^ { case id ~ _ ~ exp => AssignmentExp(id, exp) }
	}
	
	lazy val parenthesized_expression: Parser[ParenthesizedExp] = {
		LeftParenToken ~ expression ~ RightParenToken ^^ { case _ ~ exp ~ _ => ParenthesizedExp(exp) } 
	}
	
	lazy val tuple_expression: Parser[TupleExp] = {
		(LeftParenToken ~ repsep(tuple_element, CommaToken) ~ RightParenToken).flatMap({ case _ ~ list ~ _ => if(list.length == 1) { failure("cannot have only 1 item") } else { success(TupleExp(list)) }})
	}
	
	lazy val tuple_element: Parser[TupleElement] = {
		identifier ~ ColonToken ~ expression ^^ { case name ~ _ ~  exp => IdentifierColonExpTuple(name, exp) } |
		expression ^^ { case exp => ExpTuple(exp) }
	}
	
	lazy val implicit_member_expression: Parser[ImplicitMemberExp] = {
		dot_operator(".") ~ identifier ~ dot_operator(".") ~ postfix_expression ^^ { case _ ~ id ~ _ ~ exp => ImplicitMemberExp(IdentifierDotPostFixMember(id, exp)) } |
		dot_operator(".") ~ identifier ^^ { case _ ~ id => ImplicitMemberExp(IdentifierImplicitMember(id)) }
	}
	
	lazy val wildcard_expression: Parser[Exp] = {
		UnderscoreToken ^^^ WildcardExp
	}
	
	lazy val key_path_expression: Parser[KeyPathExp] = {
		BackSlashToken ~ opt(typ) ~ dot_operator(".") ~ rep1sep(key_path_component, operator(".")) ^^ 
			{ case _ ~ optType ~ _ ~ keyPathComponents => KeyPathExp(optType, keyPathComponents) }
	}
	
	lazy val key_path_component: Parser[KeyPathComponent] = {
		identifier ~ opt(key_path_postfixes) ^^ { case id ~ optPostfixes => IdentifierThenOptPostfixesKPC(id, optPostfixes) } |
		key_path_postfixes ^^ { case postfixes => PostfixesKPC(postfixes) }
	}
	
	lazy val key_path_postfixes: Parser[List[KeyPathPostfix]] = {
		rep1(key_path_postfix)
	}
	
	lazy val key_path_postfix: Parser[KeyPathPostfix] = {
		operator("?") ^^^ QuestionKPP |
		operator("!") ^^^ ExclamationKPP |
		SelfToken ^^^ SelfKPP |
		LeftBracketToken ~ function_call_argument_list ~ RightBracketToken ^^ { case _ ~ list ~ _ => FuncCallArgListKPP(list) }
	}
	
	//operators
	lazy val try_operator: Parser[TryModifier] = {
		TryToken ~ operator("?") ^^^ QuestionMarkTryModifier |
		TryToken ~ operator("!") ^^^ ExclamationTryModifier |
		TryToken ^^^ NoTryModifier
	}
	
	lazy val prefix_operator: Parser[Operator] = { operator }
	
	lazy val infix_operator: Parser[Operator] = { operator }
	
	lazy val operator: Parser[Operator] = {
		operator_thing ^^ { case OperatorLiteralToken(value) => Operator(value) }
	}
	
	//types
	lazy val typ: Parser[Type] = {
		primary_type ~ opt(trailer) ^^ { case first ~ second => {
			second match {
				case None => first
				case Some(ImplicitlyUnwrappedOptionalTypeThing) => ImplicitlyUnwrappedOptionalType(first)
				case Some(OptionalTypeThing) => OptionalType(first)
				case Some(MetatypeTypeThing) => MetatypeTypeType(first)
				case Some(MetatypeProtocolThing) => MetatypeProtocolType(first)
			}
		}}
	}
	
	//non-problematic types
	lazy val primary_type: Parser[Type] = {
		function_type |
		array_type |
		dictionary_type |
		protocol_composition_type |
		type_identifier |
		in_parens_type |
		tuple_type |
		opaque_type |
		AnyToken ^^^ AnyType |
		SelfBigToken ^^^ SelfType
	}
	
	lazy val trailer: Parser[TrailorTypeThing] = {
		operator("!") ^^^ ImplicitlyUnwrappedOptionalTypeThing |
		operator("?") ^^^ OptionalTypeThing |
		dot_operator(".") ~ TypeToken ^^^ MetatypeTypeThing |
		dot_operator(".") ~ ProtocolToken ^^^ MetatypeProtocolThing
	}
	
	lazy val function_type: Parser[FunctionType] = {
		opt(attributes) ~ function_type_arg_clause ~ opt(asynch_modifier) ~ opt(throws_modifier) ~ operator("->") ~ typ ^^ { case optAttributes ~ argClause ~ optAsync ~ optThrows ~ _ ~ theType => FunctionType(optAttributes, argClause, optAsync, optThrows, theType) }
	}
	
	lazy val function_type_arg_clause: Parser[FunctionTypeArgClause] = {
		LeftParenToken ~ RightParenToken ^^^ FunctionTypeArgClause(List()) |
		LeftParenToken ~ function_type_arg_list ~ opt(operator(".")) ~ opt(operator(".")) ~ opt(operator(".")) ~ RightParenToken ^^ { case _ ~ list ~ _ ~ _ ~ _ ~ _ => FunctionTypeArgClause(list) }
	}
	
	lazy val function_type_arg_list: Parser[List[FunctionTypeArg]] = {
		rep1sep(function_type_arg, CommaToken)
	}
	
	lazy val function_type_arg: Parser[FunctionTypeArg] = {
		identifier ~ type_annotation ^^ { case id ~ typeAnnotation => FunctionTypeArg2(id, typeAnnotation) } |
		opt(attributes) ~ opt(in_out_modifier) ~ typ ^^ { case optAttributes ~ optInOut ~ theType => FunctionTypeArg1(optAttributes, optInOut, theType) } //|
		//identifier ~ type_annotation ^^ { case id ~ typeAnnotation => FunctionTypeArg2(id, typeAnnotation) }
	}
	
	lazy val array_type: Parser[ArrayType] = {
		LeftBracketToken ~ typ ~ RightBracketToken ^^ { case _ ~ theType ~ _ => ArrayType(theType) }
	}
	
	lazy val dictionary_type: Parser[DictionaryType] = {
		LeftBracketToken ~ typ ~ ColonToken ~ typ ~ RightBracketToken ^^ { case _ ~ typ1 ~ _ ~ typ2 ~ _ => DictionaryType(typ1, typ2) }
	}
	
	lazy val type_identifier: Parser[TypeIdentifier] = {
		identifier ~ dot_operator(".") ~ type_identifier ^^ { case name ~ _ ~ nestedType => TypeIdentifier(NestedNormalType(name, nestedType)) } |
		identifier ~ generic_argument_clause ~ dot_operator(".") ~ type_identifier ^^ { case name ~ types ~ _ ~ recursive => TypeIdentifier(NestedGenericType(name, types, recursive)) } |
		identifier ~ generic_argument_clause ^^ { case name ~ types => TypeIdentifier(GenericType(name, types)) } |
		identifier ^^ { case name => TypeIdentifier(NormalType(name)) }
	}
	
	lazy val tuple_type: Parser[TupleType] = {
		LeftParenToken ~ RightParenToken ^^^ TupleType(List()) |
		LeftParenToken ~ tuple_type_element_list ~ RightParenToken ^^ { case _ ~ list ~ _ => TupleType(list) }
	}
	
	lazy val tuple_type_element_list: Parser[List[TupleTypeElement]] = {
		rep1sep(tuple_type_element, CommaToken)
	}
	
	lazy val tuple_type_element: Parser[TupleTypeElement] = {
		identifier ~ type_annotation ^^ { case id ~ typeAnnotation => TupleTypeElementNameAnnotation(id, typeAnnotation) } |
		typ ^^ { case theType => TupleTypeElementType(theType) }
	}
	
	lazy val type_annotation: Parser[TypeAnnotation] = {
		ColonToken ~ opt(attributes) ~ opt(in_out_modifier) ~ typ ^^ { case _ ~ optAttributes ~ optInOut ~ theType => TypeAnnotation(optAttributes, optInOut, theType) }
	}
	
	lazy val in_out_modifier: Parser[InOutMod] = {
		InOutToken ^^^ InOutModifier
	}
	
	lazy val protocol_composition_type: Parser[ProtocolCompositionType] = {
		type_IDs ^^ { case typeIDs => ProtocolCompositionType(typeIDs) }
	}
	
	lazy val type_IDs: Parser[List[TypeIdentifier]] = {
		//rep1sep(type_identifier, operator("&"))
		type_identifier ~ operator("&") ~ rep1sep(type_identifier, operator("&")) ^^ { case first ~ _ ~ rest => first+:rest }
	}
	
	lazy val opaque_type: Parser[OpaqueType] = {
		SomeToken ~ typ ^^ { case _ ~ theType => OpaqueType(theType) }
	}
	
	//this is here to be done in future because it relies on protocol which I haven't done yet
/* 	lazy val metatype_type: Parser[???] = {
		???
	} */
	
	lazy val in_parens_type: Parser[InParensType] = {
		LeftParenToken ~ typ ~ RightParenToken ^^ { case _ ~ theType ~ _ => InParensType(theType) }
	}
	
	//big parsers down here:
	lazy val punctuation: Parser[Punctuation] = {
		dot_operator(".") ^^^ Period |
		CommaToken ^^^ Comma |
		ColonToken ^^^ Colon |
		SemicolonToken ^^^ Semicolon |
		operator("=") ^^^ Equal |
		AtToken ^^^ At |
		HashToken ^^^ Hash |
		BackTickToken ^^^ BackTick |
		operator("?") ^^^ Question |
		operator("-") ~ operator(">") ^^^ Arrow |
		operator("&") ^^^ And |
		operator("!") ^^^ Exclamation
	}
	
	lazy val keyword: Parser[Keyword] = {
		AssociatedTypeToken ^^^ AssociatedTypeKeyword |
		ClassToken ^^^ ClassKeyword |
		DeinitToken ^^^ DeinitKeyword |
		EnumToken ^^^ EnumKeyword |
		ExtensionToken ^^^ ExtensionKeyword |
		FilePrivateToken ^^^ FilePrivateKeyword |
		FuncToken ^^^ FuncKeyword |
		ImportToken ^^^ ImportKeyword |
		InitToken ^^^ InitKeyword |
		InOutToken ^^^ InOutKeyword |
		InternalToken ^^^ InternalKeyword |
		LetToken ^^^ LetKeyword |
		OpenToken ^^^ OpenKeyword |
		OperatorToken ^^^ OperatorKeyword |
		PrivateToken ^^^ PrivateKeyword |
		ProtocolToken ^^^ ProtocolKeyword |
		PublicToken ^^^ PublicKeyword |
		RethrowsToken ^^^ RethrowsKeyword |
		StaticToken ^^^ StaticKeyword |
		StructToken ^^^ StructKeyword |
		SubscriptToken ^^^ SubscriptKeyword |
		TypeAliasToken ^^^ TypeAliasKeyword |
		VarToken ^^^ VarKeyword |
		BreakToken ^^^ BreakKeyword |
		ContinueToken ^^^ ContinueKeyword |
		DefaultToken ^^^ DefaultKeyword |
		DeferToken ^^^ DeferKeyword |
		DoToken ^^^ DoKeyword |
		ElseToken ^^^ ElseKeyword |
		FallthroughToken ^^^ FallthroughKeyword |
		ForToken ^^^ ForKeyword |
		GuardToken ^^^ GuardKeyword |
		IfToken ^^^ IfKeyword |
		InToken ^^^ InKeyword |
		RepeatToken ^^^ RepeatKeyword |
		ReturnToken ^^^ ReturnKeyword |
		SwitchToken ^^^ SwitchKeyword |
		WhereToken ^^^ WhereKeyword |
		WhileToken ^^^ WhileKeyword |
		AsToken ^^^ AsKeyword |
		AnyToken ^^^ AnyKeyword |
		CatchToken ^^^ CatchKeyword |
		FalseToken ^^^ FalseKeyword |
		IsToken ^^^ IsKeyword |
		NilToken ^^^ NilKeyword |
		SuperToken ^^^ SuperKeyword |
		SelfToken ^^^ SelfKeyword |
		SelfBigToken ^^^ SelfBigKeyword |
		ThrowToken ^^^ ThrowKeyword |
		ThrowsToken ^^^ ThrowsKeyword |
		TrueToken ^^^ TrueKeyword |
		TryToken ^^^ TryKeyword |
		UnderscoreToken ^^^ UnderscoreKeyword |
		AvailableToken ^^^ AvailableKeyword |
		HashColorLiteralToken ^^^ HashColorLiteralKeyword |
		HashColumnToken ^^^ HashColumnKeyword |
		HashElseToken ^^^ HashElseKeyword |
		HashElseIfToken ^^^ HashElseIfKeyword |
		ErrorToken ^^^ ErrorKeyword |
		HashFileToken ^^^ HashFileKeyword |
		HashFileIDToken ^^^ HashFileIDKeyword |
		HashFileLiteralToken ^^^ HashFileLiteralKeyword |
		HashFilePathToken ^^^ HashFilePathKeyword |
		HashFunctionToken ^^^ HashFunctionKeyword |
		HashIfToken ^^^ HashIfKeyword |
		HashImageLiteralToken ^^^ HashImageLiteralKeyword |
		HashLineToken ^^^ HashLineKeyword |
		HashSelectorToken ^^^ HashSelectorKeyword |
		SourceLocationToken ^^^ SourceLocationKeyword |
		WarningToken ^^^ WarningKeyword
	}
}