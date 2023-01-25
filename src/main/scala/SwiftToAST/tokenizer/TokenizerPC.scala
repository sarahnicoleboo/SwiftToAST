package SwiftToAST.tokenizer

import scala.util.parsing.combinator._

class TokenizerException(message: String) extends Exception(message)

object TokenizerPC extends RegexParsers {
	
	override def skipWhitespace = true
	override val whiteSpace = "[ \t\r\f]+".r	//not including \n on purpose
	
/* 	def as = "as" ^^ (_ => AsToken)
	def alpha = "alpha" ^^ (_ => AlphaToken) */
	
	def reservedWords: Parser[Token] = {
		"as" ^^ (_ => AsToken) | "alpha" ^^ (_ => AlphaToken)
	}
	
	def variable: Parser[VariableToken] = {
		"[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => VariableToken(str) }
	}
	
/* 	def implicit_parameter: Parser[ImplicitParameterToken] = {
		"[$][0-9]+".r ^^ { str => ImplicitParameterToken(str) }
	}
	
	def property_wrapper_projection: Parser[PropertyWrapperProjectionToken] = {
		"[$][a-zA-Z0-9_]+".r ^^ { str => PropertyWrapperProjectionToken(str) }
	} */
	
	def implicit_parameter_OR_property_wrapper_projection: Parser[ImplicitParameterOrPropertyWrapperProjectionToken] = {
		"[$][a-zA-Z0-9_]+".r ^^ { str => ImplicitParameterOrPropertyWrapperProjectionToken(str) }
	}
	
	def tokens: Parser[List[Token]] = {
		phrase(rep1(reservedWords | variable | implicit_parameter_OR_property_wrapper_projection)) ^^ { rawTokens => tokenize(rawTokens) }	//questionable
	}
	
	def tokenize(tokens: List[Token]): List[Token] = tokens
	
	
	def apply(code: String): List[Token] = {
		parse(tokens, code) match {
			case NoSuccess(message, next) => throw new TokenizerException(message)
			case Success(result, next) => result
		}
	} 
}