package SwiftToAST.tokenizer

import scala.util.parsing.combinator._

//trait WorkflowCompilationError
//case class TokenizerError(message: String) extends WorkflowCompilationError

class TokenizerError(message: String) extends Exception(message)

object TokenizerPC extends RegexParsers {
	
	override def skipWhitespace = true
	override val whiteSpace = "[ \t\r\f]+".r	//not including \n on purpose
	
	def as = "as" ^^ (_ => AsToken)
	
	def variable: Parser[VariableToken] = {
		"[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => VariableToken(str) }
	}
	
	def tokens: Parser[List[Token]] = {
		phrase(rep1(as | variable)) ^^ { rawTokens => tokenize(rawTokens) }	//questionable
	}
	
	def tokenize(tokens: List[Token]): List[Token] = tokens
	
	
	def apply(code: String): List[Token] = {
		parse(tokens, code) match {
			case NoSuccess(message, next) => throw new TokenizerError(message)
			case Success(result, next) => result
		}
	} 
}