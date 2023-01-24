package SwiftToAST.tokenizer

//import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.FlatSpec

class TokenizerTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	//import SwiftToAst.tokenizer.Tokenizer._
	
	val theTokenizer = new Tokenizer("as ")
	
	"tokenizeReservedWord" should "handle as followed by a white space" in {
		assertResult(Some(AsToken, 3)) { theTokenizer.tokenizeReservedWord(0) }
	}
	
	"TokenizerPC" should "handle an as token" in {
		assertResult(List(AsToken)) { TokenizerPC("as") }
	}
}
