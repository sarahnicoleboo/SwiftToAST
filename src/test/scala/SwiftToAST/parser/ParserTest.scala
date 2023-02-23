package SwiftToAST.parser

import org.scalatest.FlatSpec

class ParserTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	import SwiftToAST.parser._
	
	//primary_expression
	"primary_expression" should "handle an identifier followed by a list of generic types: name<Int>" in {
		val input = Seq(VariableToken("name"), OperatorLiteralToken("<"), VariableToken("Int"), OperatorLiteralToken(">"))
		val typeList = GenericArgumentClause(List(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int")))))))
		val expected = GenericVariableExp(IdentifierExp(VariableExp(Variable("name"))), typeList) 
		assertResult(expected) { Parser(Parser.primary_expression, input) }
	}
	
	"primary_expression" should "handle an identifier followed by a list of generic types: name<Int, String>" in {
		val input = Seq(VariableToken("name"), OperatorLiteralToken("<"), VariableToken("Int"), CommaToken, VariableToken("String"), OperatorLiteralToken(">"))
		val typeList = GenericArgumentClause(List(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))), TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("String")))))))
		val expected = GenericVariableExp(IdentifierExp(VariableExp(Variable("name"))), typeList) 
		assertResult(expected) { Parser(Parser.primary_expression, input) }
	}
	
	
	//identifier
	"identifier" should "handle a VariableExp identifier" in {
		val input = Seq(VariableToken("variableName"))
		assertResult(IdentifierExp(VariableExp(Variable("variableName")))) { Parser(Parser.identifier, input) }
	}
	
	"identifier" should "handle an implicit parameter OR property wrapper projction identifier: $4" in {
		val input = Seq(ImplicitParameterOrPropertyWrapperProjectionToken("$4"))
		val expected = IdentifierExp(ImplicitParameterExpOrPWP("$4")) 
		assertResult(expected) { Parser(Parser.primary_expression, input) }
	}
	
	"identifier" should "handle a property wrapper projection identifier: $abc" in {
		val input = Seq(PropertyWrapperProjectionToken("$abc"))
		val expected = IdentifierExp(PropertyWrapperProjectionExp("$abc")) 
		assertResult(expected) { Parser(Parser.primary_expression, input) }
	}
	
	//literal_expression
	//(literal)
	"literal_expression" should "handle an unsigned decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.literal_expression, input) }
	}
	
	//(array_literal)
	//add here
	
	//literal
	//(numeric_literal)
	"literal" should "handle an unsigned decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.literal, input) }
	}
	
	//(string_literal)
	"literal" should "handle a single line string literal token: hello" in {
		val input = Seq(SingleLineStringLiteralToken("hello"))
		assertResult(SingleLineStringLiteralExp("hello")) { Parser(Parser.literal, input) }
	}
	
	//(boolean_literal)
	"literal" should "handle a boolean literal true token" in {
		val input = Seq(TrueToken)
		assertResult(TrueExp) { Parser(Parser.literal, input) }
	}
	
	//(nil_literal)
	"literal" should "handle a nil token" in {
		val input = Seq(NilToken)
		assertResult(NilExp) { Parser(Parser.literal, input) }
	}
	
	//array_literal
/* 	"array_literal" should "handle an array literal []" in {
		val input = Seq(LeftBracketToken, RightBracketToken)
		assertResult(ArrayLiteralExp(List())) { Parser(Parser.array_literal, input) }
	} */
	
/* 	"array_literal" should "handle an array literal [1]" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), RightBracketToken)
		val expected = ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1"))))
		assertResult(expected) { Parser(Parser.array_literal, input) }
	} */
	
	//comma_sep_exps
	"comma_sep_exps" should "handle and empty list" in {
		val input = Seq(LeftBracketToken, RightBracketToken)
		assertResult(List()) { Parser(Parser.comma_sep_exps, input) }
	}
	
	
/* 	"The parser" should "handle an array literal [1,] and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1"))))))))) { Parser(Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, RightBracketToken)) }
	}
	
	"The parser" should "handle an array literal [1,2] and return a postfix exp" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))))))))) { Parser(Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, DecimalIntegerLiteralToken("2"), RightBracketToken)) }
	} */
	
	//numeric_literal
	"numeric_literal" should "handle an unsigned decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.numeric_literal, input) }
	}

	"numeric_literal" should "handle a negative decimal integer literal token: -23" in {
		val input = Seq(OperatorLiteralToken("-"), DecimalIntegerLiteralToken("23"))
		assertResult(PrefixExp(Operator("-"), NumericLiteralExp("23"))) { Parser(Parser.numeric_literal, input) }
	}
	
	"numeric_literal" should "handle an unsigned decimal float literal token: 2.3" in {
		val input = Seq(FloatDecimalLiteralToken("2.3"))
		assertResult(NumericLiteralExp("2.3")) { Parser(Parser.numeric_literal, input) }
	}
	
	"numeric_literal" should "handle a negative decimal float literal token: -2.3" in {
		val input = Seq(OperatorLiteralToken("-"), FloatDecimalLiteralToken("2.3"))
		assertResult(PrefixExp(Operator("-"), NumericLiteralExp("2.3"))) { Parser(Parser.numeric_literal, input) }
	}
	
	//integer_literal
	//(decimal_integer)
	"integer_literal" should "handle a decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.integer_literal, input) }
	}
	
	//(binary_integer)
	"integer_literal" should "handle a binary integer literal token: 0b0101" in {
		val input = Seq(BinaryIntegerLiteralToken("0b0101"))
		assertResult(NumericLiteralExp("0b0101")) { Parser(Parser.integer_literal, input) }
	}
	
	//(octal_integer)
	"integer_literal" should "handle an octal integer literal token: 0o734" in {
		val input = Seq(OctalIntegerLiteralToken("0o734"))
		assertResult(NumericLiteralExp("0o734")) { Parser(Parser.integer_literal, input) }
	}
	
	//(hex_integer)
	"integer_literal" should "handle a hex integer literal token: 0xA43B" in {
		val input = Seq(HexIntegerLiteralToken("0xA43B"))
		assertResult(NumericLiteralExp("0xA43B")) { Parser(Parser.integer_literal, input) }
	}
	
	//float_literal
	//(decimal_float)
	"float_literal" should "handle a decimal float literal token: 34.5" in {
		val input = Seq(FloatDecimalLiteralToken("34.5"))
		assertResult(NumericLiteralExp("34.5")) { Parser(Parser.float_literal, input) }
	}
	
	//(hex_float)
	"float_literal" should "handle a hex float literal token: 0xA34.B5" in {
		val input = Seq(FloatHexLiteralToken("0xA34.B5"))
		assertResult(NumericLiteralExp("0xA34.B5")) { Parser(Parser.float_literal, input) }
	}
	
	//string_literal
	//(single_line_string)
	"string_literal" should "handle a single line string literal token: hello" in {
		val input = Seq(SingleLineStringLiteralToken("hello"))
		assertResult(SingleLineStringLiteralExp("hello")) { Parser(Parser.string_literal, input) }
	}
	
	//(multi_line_string)
	"string_literal" should "handle a multi line string literal token: hello\nthere" in {
		val input = Seq(MultiLineStringLiteralToken("hello\nthere"))
		assertResult(MultiLineStringLiteralExp("hello\nthere")) { Parser(Parser.string_literal, input) }
	}
	
	//boolean_literal
	"boolean_literal" should "handle a boolean literal true token" in {
		val input = Seq(TrueToken)
		assertResult(TrueExp) { Parser(Parser.boolean_literal, input) }
	}
	
	"boolean_literal" should "handle a boolean literal false token" in {
		val input = Seq(FalseToken)
		assertResult(FalseExp) { Parser(Parser.boolean_literal, input) }
	}
	
	//nil_literal
	"nil_literal" should "handle a nil token" in {
		val input = Seq(NilToken)
		assertResult(NilExp) { Parser(Parser.nil_literal, input) }
	}
	
/* 	
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
	
	
	//self expressions
	"The parser" should "handle a single self expression" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(SelfExp(SelfSolo)))))) { Parser(Seq(SelfToken)) }
	}
	
	"The parser" should "handle a self method expression: self.hello" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(SelfExp(SelfMethod(IdentifierExp(VariableExp(Variable("hello")))))))))) { Parser(Seq(SelfToken, OperatorLiteralToken("."), VariableToken("hello"))) }
	}
	
	"The parser" should "handle a self subscript expression: self [2]" in {
		val list = List(ExpFunctionCallArgument(PostfixExp(NumericLiteralExp("2"))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(SelfExp(SelfSubscript(list))))))
		assertResult(expected) { Parser(Seq(SelfToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), RightBracketToken)) }
	}
	
	"The parser" should "handle a self subscript expression: self [name : 5]" in {
		val input = Seq(SelfToken, LeftBracketToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(IdentifierColonExpFunctionCallArgument(IdentifierExp(VariableExp(Variable("name"))), PostfixExp(NumericLiteralExp("5"))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(SelfExp(SelfSubscript(list))))))
		assertResult(expected) { Parser(input) }
	}
	
	"The parser" should "handle a self subscript expression: self [2, name : 5]" in {
		val input = Seq(SelfToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), CommaToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(PostfixExp(NumericLiteralExp("2"))) , IdentifierColonExpFunctionCallArgument(IdentifierExp(VariableExp(Variable("name"))), PostfixExp(NumericLiteralExp("5"))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(SelfExp(SelfSubscript(list))))))
		assertResult(expected) { Parser(input) }
	}
	
	"The parser" should "handle a self init expression" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(SelfExp(SelfInit)))))) { Parser(Seq(SelfToken, OperatorLiteralToken("."), InitToken)) }
	}

	
	//super expressions
	"The parser" should "handle a super method expression: super.hello" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(SuperExp(SuperMethod(IdentifierExp(VariableExp(Variable("hello")))))))))) { Parser(Seq(SuperToken, OperatorLiteralToken("."), VariableToken("hello"))) }
	}
	
	"The parser" should "handle a super init expression" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(SuperExp(SuperInit)))))) { Parser(Seq(SuperToken, OperatorLiteralToken("."), InitToken)) }
	}
	
	"The parser" should "handle a super subscript expression: super [2, name : 5]" in {
		val input = Seq(SuperToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), CommaToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(PostfixExp(NumericLiteralExp("2"))) , IdentifierColonExpFunctionCallArgument(IdentifierExp(VariableExp(Variable("name"))), PostfixExp(NumericLiteralExp("5"))))
		val expected = Program(Seq(ExpressionStmt(PostfixExp(SuperExp(SuperSubscript(list))))))
		assertResult(expected) { Parser(input) }
	}
	
	
	//closure expressions
	
	
	//parenthesized expressions
	"The parser" should "handle a parenthesized expression: (12)" in {
		assertResult(Program(Seq(ExpressionStmt(PostfixExp(ParenthesizedExp(PostfixExp(NumericLiteralExp("12")))))))) { Parser(Seq(LeftParenToken, DecimalIntegerLiteralToken("12"), RightParenToken)) }
	} */
}