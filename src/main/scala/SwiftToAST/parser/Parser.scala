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
	
	lazy val implicit_parameter: Parser[ImplicitParameterToken] = {
		accept("implicit_parameter", { case id @ ImplicitParameterToken(name) => id })
	}
	
	lazy val property_wrapper_projection: Parser[PropertyWrapperProjectionToken] = {
		accept("property_wrapper_projection", { case id @ PropertyWrapperProjectionToken(name) => id })
	}
	
	lazy val ip_OR_pw: Parser[ImplicitParameterOrPropertyWrapperProjectionToken] = {
		accept("ip_OR_pw", { case id @ ImplicitParameterOrPropertyWrapperProjectionToken(name) => id })
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
		
	class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
	override def first: Token = tokens.head
	override def atEnd: Boolean = tokens.isEmpty
	override def pos: Position = NoPosition
	override def rest: Reader[Token] = new TokenReader(tokens.tail)
	}
	
	def apply(tokens: Seq[Token]): Program = {
		val reader = new TokenReader(tokens)
		program(reader) match {
			case NoSuccess(message, next) => throw new ParserException(message)
			case Success(result, next) => result
		}
	}
	
	def program: Parser[Program] = phrase(block)
	
	def block: Parser[Program] = {
		rep(statement) ^^ { case stmts => Program(stmts)}
	}
	
	//statements
	lazy val statement: Parser[Stmt] = {
		test_stmt | expression_stmt
	}
	
	lazy val test_stmt: Parser[Stmt] = {
		ForToken ^^^ TestStmt 
	}
	
	lazy val expression_stmt: Parser[Stmt] = {
		expression ^^ { case exp => ExpressionStmt(exp) }
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
		literal_expression
	}
	
	lazy val identifier: Parser[Exp] = {
		variable ^^ { case VariableToken(name) => VariableExp(Variable(name)) } |
		implicit_parameter ^^ { case ImplicitParameterToken(name) => ImplicitParameterExp(name) } |
		property_wrapper_projection ^^ { case PropertyWrapperProjectionToken(name) => PropertyWrapperProjectionExp(name) }
	}
	
	lazy val generic_argument_clause: Parser[GenericArgumentClause] = {
		operator("<") ~ comma_sep_types ~ operator(">") ^^ { case _ ~ typs ~ _ => GenericArgumentClause(typs) }
	}
	
	def operator(expected: String): Parser[Operator] = {
		operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure("dfsd"))
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
	
	lazy val comma_sep_types: Parser[List[Type]] = {
		rep1sep(typ, CommaToken)
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
		//LeftBracketToken ~ expression ~ opt(CommaToken) ~ RightBracketToken ^^ { case _ ~ exp ~ _ ~ _ => ArrayLiteralExp(List(exp)) } |
		(LeftBracketToken ~ comma_sep_exps ~ opt(CommaToken) ~ RightBracketToken).flatMap({ case _ ~ expList ~ maybe ~ _ => if(expList.isEmpty && maybe.nonEmpty) { failure("dfsdf") } else { success(ArrayLiteralExp(expList)) }})
	}
	
	lazy val dictionary_literal: Parser[Exp] = {
		LeftBracketToken ~ SemicolonToken ~ RightBracketToken ^^^ DictionaryLiteralExp(List())
		LeftBracketToken ~ comma_sep_dictionary ~ opt(CommaToken) ~ RightBracketToken ^^ { case _ ~ list ~ _ ~ _ => DictionaryLiteralExp(list) }
	}
	
	lazy val comma_sep_dictionary: Parser[List[(Exp, Exp)]] = {
		rep1sep(expression ~ ColonToken ~ expression, CommaToken).map(_.map(input => (input._1._1, input._2)))
	}
	
	lazy val comma_sep_exps: Parser[List[Exp]] = {
		repsep(expression, CommaToken)
	}
	
	lazy val playground_literal: Parser[Exp] = {
		HashColorLiteralToken ~ LeftParenToken ~ RedToken ~ ColonToken ~ expression ~ CommaToken ~ GreenToken ~ ColonToken ~ expression ~ CommaToken ~ BlueToken ~ ColonToken ~ expression ~ CommaToken ~ AlphaToken ~ ColonToken ~ expression ~ RightParenToken ^^ 
			{ case _ ~ _ ~ _ ~ _ ~ exp1 ~ _ ~ _ ~ _ ~ exp2 ~ _ ~ _ ~ _ ~ exp3 ~ _ ~ _ ~ _ ~ exp4 ~ _ => ColorPlaygroundLiteralExp(exp1, exp2, exp3, exp4) } |
		HashFileLiteralToken ~ LeftParenToken ~ ResourceNameToken ~ ColonToken ~ expression ~ RightParenToken ^^
			{ case _ ~ _ ~ _ ~ _ ~ exp ~ _ =>  FilePlaygroundLiteralExp(exp) } |
		HashImageLiteralToken ~ LeftParenToken ~ ResourceNameToken ~ ColonToken ~ expression ~ RightParenToken ^^
			{ case _ ~ _ ~ _ ~ _ ~ exp ~ _ => ImagePlaygroundLiteralExp(exp) }
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
	
	lazy val typ: Parser[Type] = {
		//function_type |
		array_type |
		dictionary_type |
		type_identifier
		//tuple_type
		???
	}
	
	lazy val function_type: Parser[FunctionType] = {
		???
	}
	
	lazy val array_type: Parser[ArrayType] = {
		LeftBracketToken ~ typ ~ RightBracketToken ^^ { case _ ~ theType ~ _ => ArrayType(theType) }
	}
	
	lazy val dictionary_type: Parser[DictionaryType] = {
		LeftBracketToken ~ typ ~ ColonToken ~ typ ~ RightBracketToken ^^ { case _ ~ typ1 ~ _ ~ typ2 ~ _ => DictionaryType(typ1, typ2) }
	}
	
	lazy val type_identifier: Parser[TypeIdentifier] = {
		identifier ~ generic_argument_clause ~ operator(".") ~ type_identifier ^^ { case name ~ types ~ _ ~ recursive => TypeIdentifier(NestedType(name, types, recursive)) } |
		identifier ~ generic_argument_clause ^^ { case name ~ types => TypeIdentifier(GenericType(name, types)) } |
		identifier ^^ { case name => TypeIdentifier(NormalType(name)) }
	}
	
/* 	lazy val tuple_type: Parser[???] = {
		
	} */
}