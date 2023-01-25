package SwiftToAST.tokenizer

import scala.util.parsing.combinator._

class TokenizerException(message: String) extends Exception(message)

object TokenizerPC extends RegexParsers {
	
	override def skipWhitespace = true
	override val whiteSpace = "[ \t\r\f]+".r	//not including \n on purpose
	
/* 	def as = "as" ^^ (_ => AsToken)
	def alpha = "alpha" ^^ (_ => AlphaToken) */
	
	def words: Parser[Token] = {
		"as" ^^ (_ => AsToken) | "alpha" ^^ (_ => AlphaToken)
	}
	
	def variable: Parser[VariableToken] = {
		"[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => VariableToken(str) }
	}
	
	def tokens: Parser[List[Token]] = {
		phrase(rep1(words | variable)) ^^ { rawTokens => tokenize(rawTokens) }	//questionable
	}
	
	def tokenize(tokens: List[Token]): List[Token] = tokens
	
	
	def apply(code: String): List[Token] = {
		parse(tokens, code) match {
			case NoSuccess(message, next) => throw new TokenizerException(message)
			case Success(result, next) => result
		}
	} 
}