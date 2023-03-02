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
	
	//(identifier) thru primary_expression
	"primary_expression" should "handle a VariableExp identifier" in {
		val input = Seq(VariableToken("variableName"))
		assertResult(IdentifierExp(VariableExp(Variable("variableName")))) { Parser(Parser.primary_expression, input) }
	}
	
	//(literal_expression) thru primary_expression
	"primary_expression" should "handle an unsigned decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.primary_expression, input) }
	}
	
	//(self_expression) thru primary_expression
	"primary_expression" should "handle a single self expression" in {
		assertResult(SelfExp(SelfSolo)) { Parser(Parser.primary_expression, Seq(SelfToken)) }
	}
	
	
	//(superclass_expression) thru primary_expression
	"primary_expression" should "handle a super method expression: super.hello" in {
		val input = Seq(SuperToken, OperatorLiteralToken("."), VariableToken("hello"))
		val expected = SuperExp(SuperMethod(IdentifierExp(VariableExp(Variable("hello"))))) 
		assertResult(expected) { Parser(Parser.primary_expression, input) }
	}
	
	//(closure_expression) thru primary_expression
	
	
	//(parenthesized_expression) thru primary_expression
	
	
	//(tuple_expression) thru primary_expression
	
	
	//(implicit_member_expression) thru primary expression
	
	
	//(wilcard_expression) thru primary_expression
	
	
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
	//(literal) thru literal_expression
	"literal_expression" should "handle an unsigned decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.literal_expression, input) }
	}
	
	//(array_literal) thru literal_expression
	"literal_expression" should "handle an array literal []" in {
		val input = Seq(LeftBracketToken, RightBracketToken)
		val emptyList: List[Exp] = List()
		val expected = ArrayLiteralExp(emptyList)
		assertResult(ArrayLiteralExp(emptyList)) { Parser(Parser.literal_expression, input) }
	} 
	
	
	//(dictionary_literal) thru literal_expression
	"literal_expression" should "handle an empty dictionary literal: [:]" in {
		val input = Seq(LeftBracketToken, ColonToken, RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List()
		val expected = DictionaryLiteralExp(dictionaryList)
		assertResult(expected) { Parser(Parser.literal_expression, input) }
	}
	
	
	//(playground_literal) thrue literal_expression
	"literal_expression" should "handle a colorLiteral playground literal" in {
		val input = Seq(HashColorLiteralToken, LeftParenToken, RedToken, ColonToken, DecimalIntegerLiteralToken("0"), CommaToken, GreenToken, ColonToken, DecimalIntegerLiteralToken("1"), CommaToken, BlueToken, ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, AlphaToken, ColonToken, DecimalIntegerLiteralToken("3"), RightParenToken)
		val expected = ColorPlaygroundLiteralExp(PostfixExp(NumericLiteralExp("0")), PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2")), PostfixExp(NumericLiteralExp("3")))
		assertResult(expected) { Parser(Parser.literal_expression, input) }
	}
	
	//all the other parsers inside of literal_expression
	"literal_expression" should "handle a HashFileToken and return a HashFileExp" in {
		assertResult(HashFileExp) { Parser(Parser.literal_expression, Seq(HashFileToken)) }
	}
	
	"literal_expression" should "handle a HashFileIDToken and return a HashFileIDExp" in {
		assertResult(HashFileIDExp) { Parser(Parser.literal_expression, Seq(HashFileIDToken)) }
	}
	
	"literal_expression" should "handle a HashFilePathToken and return a HashFilePathExp" in {
		assertResult(HashFilePathExp) { Parser(Parser.literal_expression, Seq(HashFilePathToken)) }
	}
	
	"literal_expression" should "handle a HashLineToken and return a HashLineExp" in {
		assertResult(HashLineExp) { Parser(Parser.literal_expression, Seq(HashLineToken)) }
	}
	
	"literal_expression" should "handle a HashColumnToken and return a HashColumnExp" in {
		assertResult(HashColumnExp) { Parser(Parser.literal_expression, Seq(HashColumnToken)) }
	}
	
	"literal_expression" should "handle a HashFunctionToken and return a HashFunctionExp" in {
		assertResult(HashFunctionExp) { Parser(Parser.literal_expression, Seq(HashFunctionToken)) }
	}
	
	"literal_expression" should "handle a HashDSOHandleToken and return a HashDSOHandleExp" in {
		assertResult(HashDSOHandleExp) { Parser(Parser.literal_expression, Seq(HashDSOHandleToken)) }
	}
	
	//self_expression
	"self_expression" should "handle a single self expression" in {
		assertResult(SelfExp(SelfSolo)) { Parser(Parser.self_expression, Seq(SelfToken)) }
	}
	
	"self_expression" should "handle a self method expression: self.hello" in {
		val input = Seq(SelfToken, OperatorLiteralToken("."), VariableToken("hello"))
		val expected = SelfExp(SelfMethod(IdentifierExp(VariableExp(Variable("hello")))))
		assertResult(expected) { Parser(Parser.self_expression, input) }
	}
	
	"self_expression" should "handle a self subscript expression: self [2]" in {
		val input = Seq(SelfToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(PostfixExp(NumericLiteralExp("2"))))
		val expected = SelfExp(SelfSubscript(list))
		assertResult(expected) { Parser(Parser.self_expression, input) }
	}
	
	"self_expression" should "handle a self subscript expression: self [name : 5]" in {
		val input = Seq(SelfToken, LeftBracketToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(IdentifierColonExpFunctionCallArgument(IdentifierExp(VariableExp(Variable("name"))), PostfixExp(NumericLiteralExp("5"))))
		val expected = SelfExp(SelfSubscript(list))
		assertResult(expected) { Parser(Parser.self_expression, input) }
	}
	
	"self_expression" should "handle a self subscript expression: self [2, name : 5]" in {
		val input = Seq(SelfToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), CommaToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(PostfixExp(NumericLiteralExp("2"))) , IdentifierColonExpFunctionCallArgument(IdentifierExp(VariableExp(Variable("name"))), PostfixExp(NumericLiteralExp("5"))))
		val expected = SelfExp(SelfSubscript(list))
		assertResult(expected) { Parser(Parser.self_expression, input) }
	}
	
	"self_expression" should "handle a self init expression" in {
		assertResult(SelfExp(SelfInit)) { Parser(Parser.self_expression, Seq(SelfToken, OperatorLiteralToken("."), InitToken)) }
	}
	
	//superclass_expression
	"superclass_expression" should "handle a super method expression: super.hello" in {
		val input = Seq(SuperToken, OperatorLiteralToken("."), VariableToken("hello"))
		val expected = SuperExp(SuperMethod(IdentifierExp(VariableExp(Variable("hello"))))) 
		assertResult(expected) { Parser(Parser.superclass_expression, input) }
	}
	
	"superclass_expression" should "handle a super init expression" in {
		assertResult(SuperExp(SuperInit)) { Parser(Parser.superclass_expression, Seq(SuperToken, OperatorLiteralToken("."), InitToken)) }
	}
	
	"superclass_expression" should "handle a super subscript expression: super [2, name : 5]" in {
		val input = Seq(SuperToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), CommaToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(PostfixExp(NumericLiteralExp("2"))) , IdentifierColonExpFunctionCallArgument(IdentifierExp(VariableExp(Variable("name"))), PostfixExp(NumericLiteralExp("5"))))
		val expected = SuperExp(SuperSubscript(list))
		assertResult(expected) { Parser(Parser.superclass_expression, input) }
	}
	
	//closure_expression
	"closure_expression" should "handle {}" in {
		val input = Seq(LeftCurlyToken, RightCurlyToken)
		val expected = ClosureExp(None, None, None)
		assertResult(expected) { Parser(Parser.closure_expression, input) }
	}
	
	"closure_expression" should "handle { @name }" in {
		val input = Seq(LeftCurlyToken, AtToken, VariableToken("name"), RightCurlyToken)
		val emptyList: List[BalancedToken] = List()
		val attributeList = List(Attribute(IdentifierExp(VariableExp(Variable("name"))), emptyList))
		val expected = ClosureExp(Some(attributeList), None, None)
		assertResult(expected) { Parser(Parser.closure_expression, input) }
	}
	
	"closure_expression" should "handle {  }" in {
		val input = Seq(LeftCurlyToken, RightCurlyToken)
		val expected = ClosureExp(None, None, None)
		assertResult(expected) { Parser(Parser.closure_expression, input) }
	}
	//more still
	
	//helper: closure_signature
	
	
	//helper: capture_list
	
	
	//helper: capture_list_item
	
	
	//helper: capture_specifier
	
	
	//helper: closure_parameter_clause
	
	
	//helper: closure_parameter
	
	
	
	
	//parenthesized_expression
	
	
	//tuple_expression
	
	
	//implicit_member_expression
	
	
	//wildcard_expression
	
	
	//key_path_expression
	
	
	
	//literal
	//(numeric_literal) thru literal
	"literal" should "handle an unsigned decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.literal, input) }
	}
	
	//(string_literal) thru literal
	"literal" should "handle a single line string literal token: hello" in {
		val input = Seq(SingleLineStringLiteralToken("hello"))
		assertResult(SingleLineStringLiteralExp("hello")) { Parser(Parser.literal, input) }
	}
	
	//(boolean_literal) thru literal
	"literal" should "handle a boolean literal true token" in {
		val input = Seq(TrueToken)
		assertResult(TrueExp) { Parser(Parser.literal, input) }
	}
	
	//(nil_literal) thru literal
	"literal" should "handle a nil token" in {
		val input = Seq(NilToken)
		assertResult(NilExp) { Parser(Parser.literal, input) }
	}
	
	
	//array_literal
	"array_literal" should "handle an array literal []" in {
		val input = Seq(LeftBracketToken, RightBracketToken)
		val emptyList: List[Exp] = List()
		val expected = ArrayLiteralExp(emptyList)
		assertResult(ArrayLiteralExp(emptyList)) { Parser(Parser.array_literal, input) }
	} 
	
	"array_literal" should "handle an array literal [1]" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), RightBracketToken)
		val expected = ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1"))))
		assertResult(expected) { Parser(Parser.array_literal, input) }
	}
	
	"array_literal" should "handle an array literal [1,]" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, RightBracketToken)
		val expected = ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1"))))
		assertResult(expected) { Parser(Parser.array_literal, input) }
	}
	
	"array_literal" should "handle an array literal [1,2]" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, DecimalIntegerLiteralToken("2"), RightBracketToken)
		val expected = ArrayLiteralExp(List(PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))))
		assertResult(expected) { Parser(Parser.array_literal, input) }
	}
	
	//helper: comma_sep_exps
	"comma_sep_exps" should "handle an empty list" in {
		val input: Seq[Token] = Seq()
		val emptyReturnList: List[Exp] = List()
		assertResult(emptyReturnList) { Parser(Parser.comma_sep_exps, input) }
	}
	
	"comma_sep_exps" should "handle a list with one element" in {
		val input = Seq(DecimalIntegerLiteralToken("1"))
		assertResult(List(PostfixExp(NumericLiteralExp("1")))) { Parser(Parser.comma_sep_exps, input) }
	}
	
	"comma_sep_exps" should "handle a list with two elements" in {
		val input = Seq(DecimalIntegerLiteralToken("1"), CommaToken, DecimalIntegerLiteralToken("2"))
		val expected = List(PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2")))
		assertResult(expected) { Parser(Parser.comma_sep_exps, input) }
	}
	
	//dictionary_literal
	"dictionary_literal" should "handle an empty dictionary literal: [:]" in {
		val input = Seq(LeftBracketToken, ColonToken, RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List()
		val expected = DictionaryLiteralExp(dictionaryList)
		assertResult(expected) { Parser(Parser.dictionary_literal, input) }
	}
	
 	"dictionary_literal" should "handle a dictionary literal: [1:2]" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))))
		val expected = DictionaryLiteralExp(dictionaryList)
		assertResult(expected) { Parser(Parser.dictionary_literal, input) }
	}
	
	
 	"dictionary_literal" should "handle an dictionary literal: [1:2,]" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))))
		val expected = DictionaryLiteralExp(dictionaryList)
		assertResult(expected) { Parser(Parser.dictionary_literal, input) }
	}
	
 	"dictionary_literal" should "handle an dictionary literal: [1:2, 3:4]" in {
		val input = Seq(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, DecimalIntegerLiteralToken("3"), ColonToken, DecimalIntegerLiteralToken("4"), RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2"))), (PostfixExp(NumericLiteralExp("3")), PostfixExp(NumericLiteralExp("4"))))
		val expected = DictionaryLiteralExp(dictionaryList)
		assertResult(expected) { Parser(Parser.dictionary_literal, input) }
	}
	
	//playground_literal
	"playground_literal" should "handle a colorLiteral playground literal" in {
		val input = Seq(HashColorLiteralToken, LeftParenToken, RedToken, ColonToken, DecimalIntegerLiteralToken("0"), CommaToken, GreenToken, ColonToken, DecimalIntegerLiteralToken("1"), CommaToken, BlueToken, ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, AlphaToken, ColonToken, DecimalIntegerLiteralToken("3"), RightParenToken)
		val expected = ColorPlaygroundLiteralExp(PostfixExp(NumericLiteralExp("0")), PostfixExp(NumericLiteralExp("1")), PostfixExp(NumericLiteralExp("2")), PostfixExp(NumericLiteralExp("3")))
		assertResult(expected) { Parser(Parser.playground_literal, input) }
	}
	
	"playground_literal" should "handle a fileLiteral playground literal" in {
		val input = Seq(HashFileLiteralToken, LeftParenToken, ResourceNameToken, ColonToken, DecimalIntegerLiteralToken("0"), RightParenToken)
		val expected = FilePlaygroundLiteralExp(PostfixExp(NumericLiteralExp("0")))
		assertResult(expected) { Parser(Parser.playground_literal, input) }
	}
	
	"playground_literal" should "handle an imageLiteral playground literal" in {
		val input = Seq(HashImageLiteralToken, LeftParenToken, ResourceNameToken, ColonToken, DecimalIntegerLiteralToken("0"), RightParenToken)
		val expected = ImagePlaygroundLiteralExp(PostfixExp(NumericLiteralExp("0")))
		assertResult(expected) { Parser(Parser.playground_literal, input) }
	}
	
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
	//(decimal_integer) thru integer_literal
	"integer_literal" should "handle a decimal integer literal token: 23" in {
		val input = Seq(DecimalIntegerLiteralToken("23"))
		assertResult(NumericLiteralExp("23")) { Parser(Parser.integer_literal, input) }
	}
	
	//(binary_integer) thru integer_literal
	"integer_literal" should "handle a binary integer literal token: 0b0101" in {
		val input = Seq(BinaryIntegerLiteralToken("0b0101"))
		assertResult(NumericLiteralExp("0b0101")) { Parser(Parser.integer_literal, input) }
	}
	
	//(octal_integer) thru integer_literal
	"integer_literal" should "handle an octal integer literal token: 0o734" in {
		val input = Seq(OctalIntegerLiteralToken("0o734"))
		assertResult(NumericLiteralExp("0o734")) { Parser(Parser.integer_literal, input) }
	}
	
	//(hex_integer) thru integer_literal
	"integer_literal" should "handle a hex integer literal token: 0xA43B" in {
		val input = Seq(HexIntegerLiteralToken("0xA43B"))
		assertResult(NumericLiteralExp("0xA43B")) { Parser(Parser.integer_literal, input) }
	}
	
	//float_literal
	//(decimal_float) thru float_literal
	"float_literal" should "handle a decimal float literal token: 34.5" in {
		val input = Seq(FloatDecimalLiteralToken("34.5"))
		assertResult(NumericLiteralExp("34.5")) { Parser(Parser.float_literal, input) }
	}
	
	//(hex_float) thru float_literal
	"float_literal" should "handle a hex float literal token: 0xA34.B5" in {
		val input = Seq(FloatHexLiteralToken("0xA34.B5"))
		assertResult(NumericLiteralExp("0xA34.B5")) { Parser(Parser.float_literal, input) }
	}
	
	//string_literal
	"string_literal" should "handle a single line string literal token: hello" in {
		val input = Seq(SingleLineStringLiteralToken("hello"))
		assertResult(SingleLineStringLiteralExp("hello")) { Parser(Parser.string_literal, input) }
	}
	
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
	
	
	//typ
	//(function_type) thru typ
	"typ" should "handle () -> Int" in {
		val input = Seq(LeftParenToken, RightParenToken, OperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List()
		val expected = FunctionType(None, FunctionTypeArgClause(list), None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.typ, input) }
	}
	
	//(array_type) thru typ
	"typ" should "handle [Int]" in {
		val input = Seq(LeftBracketToken, VariableToken("Int"), RightBracketToken)
		val expected = ArrayType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.typ, input) }
	}	
	
	//(dictionary_type) thru typ
	"typ" should "handle [Int: Bool]" in {
		val input = Seq(LeftBracketToken, VariableToken("Int"), ColonToken, VariableToken("Bool"), RightBracketToken)
		val expected = DictionaryType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))), TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool"))))))
		assertResult(expected) { Parser(Parser.typ, input) }
	}
	
	//(type_identifier) thru typ
	"typ" should "handle the normal type: Int" in {
		val input = Seq(VariableToken("Int"))
		val expected = TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int")))))
		assertResult(expected) { Parser(Parser.typ, input) }
	}
	
	//(tuple_type) thru typ
	"typ" should "handle ()" in {
		val input = Seq(LeftParenToken, RightParenToken)
		val emptyList: List[TupleTypeElement] = List()
		val expected = TupleType(emptyList)
		assertResult(expected) { Parser(Parser.typ, input) }
	}
	
	//(optional_type) thru typ
	
	
	//(implicitly_unwrapped_optional_type) thru typ
	
	
	//(protocol_composition_type) thru typ
	"typ" should "handle: Int & Bool" in {
		val input = Seq(VariableToken("Int"), OperatorLiteralToken("&"), VariableToken("Bool"))
		val list = List(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))),
				   TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool"))))))
		val expected = ProtocolCompositionType(list)
		assertResult(expected) { Parser(Parser.typ, input) }
	}
	
	//(opaque_type) thru typ
	"typ" should "handle: some Int" in {
		val input = Seq(SomeToken, VariableToken("Int"))
		val expected = OpaqueType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.typ, input) }
	}
	
	//(metatype_type) thru typ
	
	
	//(AnyType)
	"typ" should "handle: Any" in {
		assertResult(AnyType) { Parser(Parser.typ, Seq(AnyToken)) }
	}
	
	//(SelfType)
	"typ" should "handle: Self" in {
		assertResult(SelfType) { Parser(Parser.typ, Seq(SelfBigToken)) }
	}
	
	//(in_parens_type) thru typ
	"typ" should "handle: (Int)" in {
		val input = Seq(LeftParenToken, VariableToken("Int"), RightParenToken)
		val expected = InParensType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.typ, input) }
	}
	
	
	//function_type
	"function_type" should "handle () -> Int" in {
		val input = Seq(LeftParenToken, RightParenToken, OperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List()
		val expected = FunctionType(None, FunctionTypeArgClause(list), None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.function_type, input) }
	}
	
	"function_type" should "handle (Bool) -> Int" in {
		val input = Seq(LeftParenToken, VariableToken("Bool"), RightParenToken, OperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List(FunctionTypeArg1(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool")))))))
		val expected = FunctionType(None, FunctionTypeArgClause(list), None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.function_type, input) }
	}
	
	"function_type" should "handle (Bool, x: String) -> Int" in {
		val input = Seq(LeftParenToken, VariableToken("Bool"), CommaToken, VariableToken("x"), ColonToken, VariableToken("String"), RightParenToken, OperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List(FunctionTypeArg1(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool")))))),
											FunctionTypeArg2(IdentifierExp(VariableExp(Variable("x"))), TypeAnnotation(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("String"))))))))
		val expected = FunctionType(None, FunctionTypeArgClause(list), None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.function_type, input) }
	}
	
	"function_type" should "handle (Bool, x: String) asynch -> Int" in {
		val input = Seq(LeftParenToken, VariableToken("Bool"), CommaToken, VariableToken("x"), ColonToken, VariableToken("String"), RightParenToken, 
					AsyncToken, OperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List(FunctionTypeArg1(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool")))))),
											FunctionTypeArg2(IdentifierExp(VariableExp(Variable("x"))), TypeAnnotation(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("String"))))))))
		val expected = FunctionType(None, FunctionTypeArgClause(list), Some(AsyncModifier), None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.function_type, input) }
	}
	
	//PROBLEM
/* 	"function_type" should "handle @name () -> Int" in {
		val input = Seq(AtToken, VariableToken("name"), LeftParenToken, RightParenToken, OperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List()
		val emptyBTList: List[BalancedToken] = List()
		val theAttribute = Attribute(IdentifierExp(VariableExp(Variable("name"))) , emptyBTList)
		val expected = FunctionType(Some(List(theAttribute)), FunctionTypeArgClause(list), None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.function_type, input) }
	} */
	
	
	//helper: function_type_arg
	"function_type_arg" should "handle x: String" in {
		val input = Seq(VariableToken("x"), ColonToken, VariableToken("String"))
		val typeAnnotation = TypeAnnotation(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("String"))))))
		val expected = FunctionTypeArg2(IdentifierExp(VariableExp(Variable("x"))), typeAnnotation)
		assertResult(expected) { Parser(Parser.function_type_arg, input) }
	}
	
	//helper: type_annotation
	"type_annotation" should "handle : String" in {
		val input = Seq(ColonToken, VariableToken("String"))
		val expected = TypeAnnotation(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("String"))))))
		assertResult(expected) { Parser(Parser.type_annotation, input) }
	}	
	
	//array_type
	"array_type" should "handle [Int]" in {
		val input = Seq(LeftBracketToken, VariableToken("Int"), RightBracketToken)
		val expected = ArrayType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.array_type, input) }
	}	
	
	//dictionary_type
	"dictionary_type" should "handle [Int: Bool]" in {
		val input = Seq(LeftBracketToken, VariableToken("Int"), ColonToken, VariableToken("Bool"), RightBracketToken)
		val expected = DictionaryType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))), TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool"))))))
		assertResult(expected) { Parser(Parser.dictionary_type, input) }
	}	
	
	//type_identifier
	"type_identifier" should "handle the normal type: Int" in {
		val input = Seq(VariableToken("Int"))
		val expected = TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int")))))
		assertResult(expected) { Parser(Parser.type_identifier, input) }
	}
	
	"type_identifier" should "handle the generic type: Int<Bool, Int>" in {
		val input = Seq(VariableToken("Int"), OperatorLiteralToken("<"), VariableToken("Bool"), CommaToken, VariableToken("Int"), OperatorLiteralToken(">"))
		val genericList: List[Type] = List(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool"))))), TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		val expected = TypeIdentifier(GenericType(IdentifierExp(VariableExp(Variable("Int"))), GenericArgumentClause(genericList)))
		assertResult(expected) { Parser(Parser.type_identifier, input) }
	}
	
	"type_identifier" should "handle the nested normal type: Number.int" in {
		val input = Seq(VariableToken("Number"), OperatorLiteralToken("."), VariableToken("int"))
		val expected = TypeIdentifier(NestedNormalType(IdentifierExp(VariableExp(Variable("Number"))), TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("int")))))))
		assertResult(expected) { Parser(Parser.type_identifier, input) }
	}
	
	//issue here with . and >.
	//we would need it to tokenize as > and . rather than >.
	"type_identifier" should "handle the nested generic type: Number<Int>.int" in {
		val input = Seq(VariableToken("Number"), OperatorLiteralToken("<"), VariableToken("Int"), OperatorLiteralToken(">"), OperatorLiteralToken("."), VariableToken("int"))
		val genericList: List[Type] = List(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		val genericArgClause = GenericArgumentClause(genericList)
		val expected = TypeIdentifier(NestedGenericType(IdentifierExp(VariableExp(Variable("Number"))), genericArgClause ,TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("int")))))))
		assertResult(expected) { Parser(Parser.type_identifier, input) }
	}
	
	//tuple_type
	"tuple_type" should "handle ()" in {
		val input = Seq(LeftParenToken, RightParenToken)
		val emptyList: List[TupleTypeElement] = List()
		val expected = TupleType(emptyList)
		assertResult(expected) { Parser(Parser.tuple_type, input) }
	}
	
	"tuple_type" should "handle (Int)" in {
		val input = Seq(LeftParenToken, VariableToken("Int"), RightParenToken)
		val list: List[TupleTypeElement] = List(TupleTypeElementType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int")))))))
		val expected = TupleType(list)
		assertResult(expected) { Parser(Parser.tuple_type, input) }
	}
	
	"tuple_type" should "handle (Int, x: Bool)" in {
		val input = Seq(LeftParenToken, VariableToken("Int"), CommaToken, VariableToken("x"), ColonToken, VariableToken("Bool"), RightParenToken)
		val list: List[TupleTypeElement] = List(TupleTypeElementType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int")))))), 
												TupleTypeElementNameAnnotation(IdentifierExp(VariableExp(Variable("x"))),
																				TypeAnnotation(None, None, TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool"))))))))
		val expected = TupleType(list)
		assertResult(expected) { Parser(Parser.tuple_type, input) }
	}
	
	//optional_type
	//left recursion
/* 	"optional_type" should "handle Int?" in {
		val input = Seq(VariableToken("Int"), OperatorLiteralToken("?"))
		val expected = OptionalType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.optional_type, input) }
	} */
	
	//implicitly_unwrapped_optional_type
	//also gonna have left recursion
	
	
	//protocol_composition_type
	"protocol_composition_type" should "handle: Int & Bool" in {
		val input = Seq(VariableToken("Int"), OperatorLiteralToken("&"), VariableToken("Bool"))
		val list = List(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))),
				   TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Bool"))))))
		val expected = ProtocolCompositionType(list)
		assertResult(expected) { Parser(Parser.protocol_composition_type, input) }
	}
	
	//opaque_type
	"opaque_type" should "handle: some Int" in {
		val input = Seq(SomeToken, VariableToken("Int"))
		val expected = OpaqueType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.opaque_type, input) }
	}
	
	
	//metatype_type
	
	
	//in_parens_type
	"in_parens_type" should "handle: (Int)" in {
		val input = Seq(LeftParenToken, VariableToken("Int"), RightParenToken)
		val expected = InParensType(TypeIdentifier(NormalType(IdentifierExp(VariableExp(Variable("Int"))))))
		assertResult(expected) { Parser(Parser.in_parens_type, input) }
	}
	
	
	//attribute & attributes
	"attribute" should "handle @varName" in {
		val input = Seq(AtToken, VariableToken("varName"))
		val emptyBTList: List[BalancedToken] = List()
		val expected = Attribute(IdentifierExp(VariableExp(Variable("varName"))) , emptyBTList)
		assertResult(expected) { Parser(Parser.attribute, input) }
	}
	
	"attribute" should "handle @varName ()" in {
		val input = Seq(AtToken, VariableToken("varName"), LeftParenToken, RightParenToken)
		val emptyBTList: List[BalancedToken] = List()
		val expected = Attribute(IdentifierExp(VariableExp(Variable("varName"))) , emptyBTList)
		assertResult(expected) { Parser(Parser.attribute, input) }
	}
	
	"attribute" should "handle @varName (xy)" in {
		val input = Seq(AtToken, VariableToken("varName"), LeftParenToken, VariableToken("xy"), RightParenToken)
		val list: List[BalancedToken] = List(IdentifierBalancedToken(IdentifierExp(VariableExp(Variable("xy")))))
		val expected = Attribute(IdentifierExp(VariableExp(Variable("varName"))) , list)
		assertResult(expected) { Parser(Parser.attribute, input) }
	}
	
	"attribute" should "handle @varName (xy Do)" in {
		val input = Seq(AtToken, VariableToken("varName"), LeftParenToken, VariableToken("xy"), DoToken, RightParenToken)
		val list: List[BalancedToken] = List(IdentifierBalancedToken(IdentifierExp(VariableExp(Variable("xy")))), KeywordBalancedToken(DoKeyword))
		val expected = Attribute(IdentifierExp(VariableExp(Variable("varName"))) , list)
		assertResult(expected) { Parser(Parser.attribute, input) }
	}
	
	"attributes" should "handle two attributes: @varName @name(xy)" in {
		val input = Seq(AtToken, VariableToken("varName"), AtToken, VariableToken("name"), LeftParenToken, VariableToken("xy"), RightParenToken)
		val emptyBTList: List[BalancedToken] = List()
		val list: List[BalancedToken] = List(IdentifierBalancedToken(IdentifierExp(VariableExp(Variable("xy")))))
		val expected: List[Attribute] = List(Attribute(IdentifierExp(VariableExp(Variable("varName"))) , emptyBTList), 
											Attribute(IdentifierExp(VariableExp(Variable("name"))) , list))
		assertResult(expected) { Parser(Parser.attributes, input) }
	}
	
	//attribute_argument_clause
	"attribute_argument_clause" should "handle one balanced token: xy" in {
		val input = Seq(LeftParenToken, VariableToken("xy"), RightParenToken)
		val expected: List[BalancedToken] = List(IdentifierBalancedToken(IdentifierExp(VariableExp(Variable("xy")))))
		assertResult(expected) { Parser(Parser.attribute_argument_clause, input) }
	}
	
	//balanced_token
	"balanced_token" should "handle an identifier: xy" in {
		val input = Seq(VariableToken("xy"))
		val expected = IdentifierBalancedToken(IdentifierExp(VariableExp(Variable("xy"))))
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_token" should "handle a keyword: Do" in {
		val input = Seq(DoToken)
		val expected = KeywordBalancedToken(DoKeyword)
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_token" should "handle a literal: 5" in {
		val input = Seq(DecimalIntegerLiteralToken("5"))
		val expected = LiteralBalancedToken(NumericLiteralExp("5"))
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_token" should "handle an operator: -" in {
		val input = Seq(OperatorLiteralToken("-"))
		val expected = OperatorBalancedToken(Operator("-"))
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_toked" should "handle an operator: -" in {
		val input = Seq(OperatorLiteralToken("-"))
		val expected = OperatorBalancedToken(Operator("-"))
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_token" should "handle a punctuation: ." in {
		val input = Seq(OperatorLiteralToken("."))
		val expected = PunctuationBalancedToken(Period)
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_token" should "handle a parenthesized punctuation: (.)" in {
		val input = Seq(LeftParenToken, OperatorLiteralToken("."), RightParenToken)
		val expected = InParensBalancedToken(PunctuationBalancedToken(Period))
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_token" should "handle a bracketed punctuation: [.]" in {
		val input = Seq(LeftBracketToken, OperatorLiteralToken("."), RightBracketToken)
		val expected = InBracketsBalancedToken(PunctuationBalancedToken(Period))
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	"balanced_token" should "handle a braced punctuation: {.}" in {
		val input = Seq(LeftCurlyToken, OperatorLiteralToken("."), RightCurlyToken)
		val expected = InBracesBalancedToken(PunctuationBalancedToken(Period))
		assertResult(expected) { Parser(Parser.balanced_token, input) }
	}
	
	
	//other shit
	//function_result
	
	
}