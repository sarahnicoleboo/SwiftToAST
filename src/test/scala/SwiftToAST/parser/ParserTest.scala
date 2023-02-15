package SwiftToAST.parser

import org.scalatest.FlatSpec

class ParserTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	import SwiftToAST.parser._
	
	"The parser" should "handle a decimal integer literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NumericLiteralExp("23")))))) { Parser(Seq(DecimalIntegerLiteralToken("23"))) }
	}
	
	"The parser" should "handle a binary integer literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NumericLiteralExp("0b0101")))))) { Parser(Seq(BinaryIntegerLiteralToken("0b0101"))) }
	}
	
	"The parser" should "handle an octal integer literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NumericLiteralExp("0o734")))))) { Parser(Seq(OctalIntegerLiteralToken("0o734"))) }
	}
	
	"The parser" should "handle a hex integer literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NumericLiteralExp("0xA43B")))))) { Parser(Seq(BinaryIntegerLiteralToken("0xA43B"))) }
	}
	
	"The parser" should "handle a decimal float literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NumericLiteralExp("34.5")))))) { Parser(Seq(FloatDecimalLiteralToken("34.5"))) }
	}
	
	"The parser" should "handle a hex float literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NumericLiteralExp("0x34.AB")))))) { Parser(Seq(FloatHexLiteralToken("0x34.AB"))) }
	}
	
	"The parser" should "handle a single line string literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(SingleLineStringLiteralExp("hello")))))) { Parser(Seq(SingleLineStringLiteralToken("hello"))) }
	}
	
	"The parser" should "handle a multi line string literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(MultiLineStringLiteralExp("hello\nthere")))))) { Parser(Seq(MultiLineStringLiteralToken("hello\nthere"))) }
	}
	
	"The parser" should "handle a boolean literal true token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(TrueExp))))) { Parser(Seq(TrueToken)) }
	}
	
	"The parser" should "handle a boolean literal false token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(FalseExp))))) { Parser(Seq(FalseToken)) }
	}
	
	"The parser" should "handle a nil token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NilExp))))) { Parser(Seq(NilToken)) }
	}
}