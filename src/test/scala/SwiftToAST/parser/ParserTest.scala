package SwiftToAST.parser

import org.scalatest.FlatSpec

class ParserTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	import SwiftToAST.parser._
	
	//just feeling my way around, these tests aren't actually useful
	"Parser" should "handle a test stmt" in {
		assertResult(Program(Seq(TestStmt))) { Parser(Seq(ForToken)) }
	}
	
	"Parser" should "handle a test VariableExp" in {
		assertResult(Program(Seq(VariableExp(Variable("hi"))))) { Parser(Seq(ForToken, VariableToken("hi"))) }
	}
}