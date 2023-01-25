package SwiftToAST.tokenizer

//import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.FlatSpec

class TokenizerTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	
	"TokenizerPC" should "fail" in {
		assertThrows[TokenizerException] { TokenizerPC("!") }		//THIS WILL FAIL ONCE I ADD SYMBOLS LATER
	}
	
	//reserved words testing
	
	"TokenizerPC" should "handle an as token" in {
		assertResult(List(AsToken)) { TokenizerPC("as") }
	}
	
	"TokenizerPC" should "handle an alpha token" in {
		assertResult(List(AlphaToken)) { TokenizerPC("alpha") }
	}
	
	"TokenizerPC" should "handle an alpha token followed by an as token" in {
		assertResult(List(AlphaToken, AsToken)) { TokenizerPC("alpha as") }
	}
	
	
	//variable testing
	
	"TokenizerPC" should "handle a variable literal with all lowercase letters" in {
		assertResult(List(VariableToken("hello"))) { TokenizerPC("hello") }
	}
	
	"TokenizerPC" should "handle a variable literal that contains digits" in {
		assertResult(List(VariableToken("h3ll0"))) { TokenizerPC("h3ll0") }
	}
	
	"TokenizerPC" should "handle a variable literal that starts with an underscore" in {
		assertResult(List(VariableToken("_h3ll0"))) { TokenizerPC("_h3ll0") }
	}
	
/* 	//implicit parameter testing
	
	"TokenizerPC" should "handle an implicit parameter literal with one digit" in {
		assertResult(List(ImplicitParameterToken("$0"))) { TokenizerPC("$0") }
	}
	
	"TokenizerPC" should "handle an implicit parameter literal with multiple digits" in {
		assertResult(List(ImplicitParameterToken("$032"))) { TokenizerPC("$032") }
	}
	
	//property wrapper projection testing
	
	"TokenizerPC" should "handle property wrapper projection with one letter" in {
		assertResult(List(PropertyWrapperProjectionToken("$a"))) { TokenizerPC("$a") }
	}
	
	"TokenizerPC" should "handle property wrapper projection that starts with a number and has multiple letters" in {
		assertResult(List(PropertyWrapperProjectionToken("$2abc"))) { TokenizerPC("$2abc") }
	} */
	
	//implicit param and property wrapper projection testing
	
	"TokenizerPC" should "handle an implicit parameter literal with one digit" in {
		assertResult(List(ImplicitParameterOrPropertyWrapperProjectionToken("$0"))) { TokenizerPC("$0") }
	}
	
	"TokenizerPC" should "handle an implicit parameter literal with multiple digits" in {
		assertResult(List(ImplicitParameterOrPropertyWrapperProjectionToken("$032"))) { TokenizerPC("$032") }
	}
	
	"TokenizerPC" should "handle property wrapper projection with one letter" in {
		assertResult(List(ImplicitParameterOrPropertyWrapperProjectionToken("$a"))) { TokenizerPC("$a") }
	}
	
	"TokenizerPC" should "handle property wrapper projection that starts with a number and has multiple letters" in {
		assertResult(List(ImplicitParameterOrPropertyWrapperProjectionToken("$2abc"))) { TokenizerPC("$2abc") }
	}
}
