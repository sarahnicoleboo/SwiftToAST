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
	
	"The parser" should "handle an array literal [] and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(ArrayLiteralExp(List())))))) { Parser(Seq(LeftBracketToken, RightBracketToken)) }
	}
	
	"The parser" should "handle an array literal [1] and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1"))))))))) { Parser(Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), RightBracketToken)) }
	}
	
	"The parser" should "handle an array literal [1,] and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1"))))))))) { Parser(Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, RightBracketToken)) }
	}
	
	"The parser" should "handle an array literal [1,2] and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))))))))) { Parser(Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, DecimalIntegerLiteralToken("2"), RightBracketToken)) }
	}
	
	"The parser" should "handle an dictionary literal [:] and return a postfix exp" in {
		val dictionaryList: List[(Exp, Exp)] = List()
		val expected = Program(Seq(ExpressionStmt(PostfixExp(DictionaryLiteralExp(dictionaryList)))))
		assertResult(expected) { Parser(Seq(LeftBracketToken, ColonToken, RightBracketToken)) }
	}
	
 	"The parser" should "handle an dictionary literal [1:2] and return a postfix exp" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(DictionaryLiteralExp(dictionaryList)))))
		assertResult(expected) { Parser(input) }
	}
	
 	"The parser" should "handle an dictionary literal [1:2,] and return a postfix exp" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(DictionaryLiteralExp(dictionaryList)))))
		assertResult(expected) { Parser(input) }
	}
	
 	"The parser" should "handle an dictionary literal [1:2, 3:4] and return a postfix exp" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, DecimalIntegerLiteralToken("3"), ColonToken, DecimalIntegerLiteralToken("4"), RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))), (PostfixExp(NumericLiteralExp("3")), PostfixExp(NumericLiteralExp("4"))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(DictionaryLiteralExp(dictionaryList)))))
		assertResult(expected) { Parser(input) }
	}
}