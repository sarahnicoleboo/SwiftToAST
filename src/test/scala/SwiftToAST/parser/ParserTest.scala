package SwiftToAST.parser

import org.scalatest.FlatSpec

class ParserTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	import SwiftToAST.parser._
	
	//PRIMARY EXPRESSIONS
	
	//identifers
	"The parser" should "handle an identifier followed by a list of generic types: name<Int>" in {
		val input = Seq(VariableToken("name"), OperatorLiteralToken("<"), VariableToken("Int"), OperatorLiteralToken(">"))
		val typeList = GenericArgumentClause(List(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int")))))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(GenericVariableExp(IdentifierExp(VariableExp(Variable("name"))), typeList))))) 
		assertResult(expected) { Parser(input) }
	}	
	
	//LITERAL EXPRESSIONS
	
	//numeric literals
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
	
	
	//string literals
	"The parser" should "handle a single line string literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(SingleLineStringLiteralExp("hello")))))) { Parser(Seq(SingleLineStringLiteralToken("hello"))) }
	}
	
	"The parser" should "handle a multi line string literal token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(MultiLineStringLiteralExp("hello\nthere")))))) { Parser(Seq(MultiLineStringLiteralToken("hello\nthere"))) }
	}
	
	
	//boolean literals
	"The parser" should "handle a boolean literal true token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(TrueExp))))) { Parser(Seq(TrueToken)) }
	}
	
	"The parser" should "handle a boolean literal false token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(FalseExp))))) { Parser(Seq(FalseToken)) }
	}
	
	
	//nil literal
	"The parser" should "handle a nil token and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(NilExp))))) { Parser(Seq(NilToken)) }
	}
	
	
	//array literals
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
	
	
	//dictionary literals
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
	
	
	//playground literals
	"The parser" should "handle a colorLiteral playground literal" in {
		val input = Seq(HashColorLiteralToken, LeftParenToken, RedToken, ColonToken, DecimalIntegerLiteralToken("0"), CommaToken, GreenToken, ColonToken, DecimalIntegerLiteralToken("1"), CommaToken, BlueToken, ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, AlphaToken, ColonToken, DecimalIntegerLiteralToken("3"), RightParenToken)
		val expected = PostfixExp(ColorPlaygroundLiteralExp(PostfixExp(NumericLiteralExp("0")), PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2")), PostfixExp(NumericLiteralExp("3"))))
		assertResult(Program(Seq(ExpressionStmt(expected)))) { Parser(input) }
	}
	
	"The parser" should "handle a fileLiteral playground literal" in {
		val input = Seq(HashFileLiteralToken, LeftParenToken, ResourceNameToken, ColonToken, DecimalIntegerLiteralToken("0"), RightParenToken)
		val expected = Program(Seq(ExpressionStmt(PostfixExp(FilePlaygroundLiteralExp(PostfixExp(NumericLiteralExp("0")))))))
		assertResult(expected) { Parser(input) }
	}
	
	"The parser" should "handle an imageLiteral playground literal" in {
		val input = Seq(HashImageLiteralToken, LeftParenToken, ResourceNameToken, ColonToken, DecimalIntegerLiteralToken("0"), RightParenToken)
		val expected = Program(Seq(ExpressionStmt(PostfixExp(ImagePlaygroundLiteralExp(PostfixExp(NumericLiteralExp("0")))))))
		assertResult(expected) { Parser(input) }
	}
	
	
	//other literals
	"The parser" should "handle a HashFileToken and return a HashFileExp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(HashFileExp))))) { Parser(Seq(HashFileToken)) }
	}
	
	"The parser" should "handle a HashFileIDToken and return a HashFileIDExp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(HashFileIDExp))))) { Parser(Seq(HashFileIDToken)) }
	}
	
	"The parser" should "handle a HashFilePathToken and return a HashFilePathExp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(HashFilePathExp))))) { Parser(Seq(HashFilePathToken)) }
	}
	
	"The parser" should "handle a HashLineToken and return a HashLineExp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(HashLineExp))))) { Parser(Seq(HashLineToken)) }
	}
	
	"The parser" should "handle a HashColumnToken and return a HashColumnExp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(HashColumnExp))))) { Parser(Seq(HashColumnToken)) }
	}
	
	"The parser" should "handle a HashFunctionToken and return a HashFunctionExp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(HashFunctionExp))))) { Parser(Seq(HashFunctionToken)) }
	}
	
	"The parser" should "handle a HashDSOHandleToken and return a HashDSOHandleExp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(HashDSOHandleExp))))) { Parser(Seq(HashDSOHandleToken)) }
	}
	
}