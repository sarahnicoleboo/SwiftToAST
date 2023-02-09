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
		accept("property_wrapper", { case id @ PropertyWrapperProjectionToken(name) => id })
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
		//val test = ForToken ~ variable ^^ { case _ ~ VariableToken(name) => VariableExp(Variable(name)) } 
		//val test2 = ForToken ^^^ TestStmt 
		//test | test2
	}
	
	lazy val test_stmt: Parser[Stmt] = {
		ForToken ^^^ TestStmt 
	}
	
	//expressions
	//expression: try_operator? prefix_expression binary_expressions?;
	lazy val expression_stmt: Parser[Stmt] = {
		opt(try_operator) ~ prefix_expression	//left off here 1
		???
	}
	
	//prefix_expression:
	// prefix_operator? postfix_expression
	// | in_out_expression;
	lazy val prefix_expression = {
		opt(prefix_operator) ~ postfix_expression //left off here 2
		???
	}
	
	//postfix_expression:
	// primary_expression (
	//	function_call_suffix
	//	| initializer_suffix
	//	| explicit_member_suffix
	//	| postfix_self_suffix
	//	| subscript_suffix
	//	| forced_value_suffix
	//	| optional_chaining_suffix
	// )* postfix_operator*?;
	lazy val postfix_expression = {
		primary_expression //left off here 3
		???
	}
	
	//primary_expression:
	// unqualified_name generic_argument_clause?
	// | array_type
	// | dictionary_type
	// | literal_expression
	// | self_expression
	// | superclass_expression
	// | closure_expression
	// | parenthesized_operator
	// | parenthesized_expression
	// | tuple_expression
	// | implicit_member_expression
	// | wildcard_expression
	// | key_path_expression
	// | selector_expression
	// | key_path_string_expression;
	lazy val primary_expression = {
		/*unqualified_name ~ opt(generic_argument_clause)*/ //left off here 4
		// | array_type
		// | dictionary_type
		literal_expression
		???
	}
	
	//unqualified_name: identifier (LPAREN argument_names RPAREN)?;
	lazy val unqualified_name = {
		identifier //left off here 5
		???
	}
	
	//identifier:
/* 	(
		LINUX
		| WINDOWS
		| ALPHA
		| ARCH
		| ARM
		| ARM64
		| ASSIGNMENT
		| BLUE
		| CAN_IMPORT
		| COMPILER
		| FILE
		| GREEN
		| HIGHER_THAN
		| I386
		| I_OS
		| OSX
		| I_OS_APPLICATION_EXTENSION
		| LINE
		| LOWER_THAN
		| MAC_CATALYST
		| MAC_CATALYST_APPLICATION_EXTENSION
		| MAC_OS
		| MAC_OS_APPLICATION_EXTENSION
		| OS
		| PRECEDENCE_GROUP
		| RED
		| RESOURCE_NAME
		| SAFE
		| SIMULATOR
		| SOME
		| SWIFT
		| TARGET_ENVIRONMENT
		| TV_OS
		| UNSAFE
		| WATCH_OS
		| X86_64

		// Keywords reserved in particular contexts
		| ASSOCIATIVITY
		| CONVENIENCE
		| DYNAMIC
		| DID_SET
		| FINAL
		| GET
		| INFIX
		| INDIRECT
		| LAZY
		| LEFT
		| MUTATING
		| NONE
		| NONMUTATING
		| OPTIONAL
		| OVERRIDE
		| POSTFIX
		| PRECEDENCE
		| PREFIX
		| PROTOCOL
		| REQUIRED
		| RIGHT
		| SET
		| TYPE
		| UNOWNED
		| WEAK
		| WILL_SET
		| IN
		| FOR
		| GUARD
		| WHERE
		| DEFAULT
		| INTERNAL
		| PRIVATE
		| PUBLIC
		| OPEN
		| AS
		| PREFIX
		| POSTFIX
		| WHILE
		| SELF
		| SELF_BIG
		| SET
		| CLASS
		| GETTER
		| SETTER
		| OPERATOR
		| DO
		| CATCH
	)
	| Identifier
	| BACKTICK (keyword | Identifier | DOLLAR) BACKTICK; */
	lazy val identifier = {
		???
	}
	
/* 	literal_expression:
	literal
	| array_literal
	| dictionary_literal
	| playground_literal
	| HASH_FILE
	| HASH_FILE_ID
	| HASH_FILE_PATH
	| HASH_LINE
	| HASH_COLUMN
	| HASH_FUNCTION
	| HASH_DSO_HANDLE; */
	lazy val literal_expression = {
		literal //left off here 6
	}
	
/* 	literal:
	numeric_literal
	| string_literal
	| boolean_literal
	| nil_literal; */
	lazy val literal: Parser[Exp] = {
		numeric_literal | string_literal |
		boolean_literal | nil_literal
	}
	
/* 	numeric_literal:
	negate_prefix_operator? integer_literal
	| negate_prefix_operator? Floating_point_literal; */
	lazy val numeric_literal: Parser[Exp] = {
		opt(MinusToken) ~ integer_literal ^^ { case optMinus ~ intLiteral => SignedNumericLiteralExp(optMinus, intLiteral) } |
		opt(MinusToken) ~ float_literal ^^ { case optMinus ~ floatLiteral => SignedNumericLiteralExp(optMinus, floatLiteral) }
	}
	
	lazy val integer_literal: Parser[NumericLiteralExp] = {
		decimal_integer ^^ { case DecimalIntegerLiteralToken(value) => DecimalIntegerLiteralExp(value) } |
		binary_integer ^^ { case BinaryIntegerLiteralToken(value) => BinaryIntegerLiteralExp(value) } |
		octal_integer ^^ { case OctalIntegerLiteralToken(value) => OctalIntegerLiteralExp(value) } |
		hex_integer ^^ { case HexIntegerLiteralToken(value) => HexIntegerLiteralExp(value) }
	}
	
	lazy val float_literal: Parser[NumericLiteralExp] = {
		decimal_float ^^ { case FloatDecimalLiteralToken(value) => DecimalFloatLiteralExp(value) } |
		hex_float ^^ { case FloatHexLiteralToken(value) => HexFloatLiteralExp(value) }
	}
	
	lazy val string_literal: Parser[Exp] = {
		single_line_string ^^ { case SingleLineStringLiteralToken(value) => SingleLineStringLiteralExp(value) } |	//interpolation(value)*/ }
		multi_line_string ^^ { case MultiLineStringLiteralToken(value) => MultiLineStringLiteralExp(value) }
	}
	
	lazy val boolean_literal = {
		TrueToken ^^^ TrueExp | FalseToken ^^^ FalseExp
	}
	
	lazy val nil_literal = {
		NilToken ^^^ NilExp
	}
	
	//operators
	lazy val try_operator = {
		TryToken ~ opt(QuestionToken) ^^ { case _ ~ optQuestion => TryOperator(optQuestion) } |
		TryToken ~ opt(ExclamationToken) ^^ { case _ ~ optExclamation => TryOperator(optExclamation) }
	}
	
	lazy val prefix_operator: Parser[List[Op]] = {
		operator
	}
	
	lazy val operator: Parser[List[Op]] = {
		rep1(ops)
	}
	
	lazy val ops: Parser[Op] = {
		DivisionToken ^^^ DivisionOp | EqualToken ^^^ EqualOp |
		MinusToken ^^^ MinusOp | AdditionToken ^^^ AdditionOp |
		ExclamationToken ^^^ NotOp | MultiplicationToken ^^^ MultiplicationOp |
		ModToken ^^^ ModOp | AndToken ^^^ AndOp |
		OrToken ^^^ OrOp | LessThanToken ^^^ LessThanOp |
		GreaterThanToken ^^^ GreaterThanOp | CaretToken ^^^ CaretOp |
		TildeToken ^^^ TildeOp | QuestionToken ^^^ QuestionOp
	}
}