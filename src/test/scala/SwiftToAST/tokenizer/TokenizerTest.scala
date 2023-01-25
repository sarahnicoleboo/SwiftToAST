package SwiftToAST.tokenizer

//import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.FlatSpec

class TokenizerTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	
	"TokenizerPC" should "handle an as token" in {
		assertResult(List(AsToken)) { TokenizerPC("as") }
	}
	
	"TokenizerPC" should "handle an alpha token" in {
		assertResult(List(AlphaToken)) { TokenizerPC("alpha") }
	}
	
	"TokenizerPC" should "handle an alpha token followed by an as token" in {
		assertResult(List(AlphaToken, AsToken)) { TokenizerPC("alpha as") }
	}
	
	"TokenizerPC" should "handle variable literal" in {
		assertResult(List(VariableToken("hello"))) { TokenizerPC("hello") }
	}
}
