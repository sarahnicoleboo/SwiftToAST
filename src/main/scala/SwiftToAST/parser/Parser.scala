package SwiftToAST.parser

import SwiftToAST.tokenizer._
import scala.util.parsing.combinator._

class ParserException(message: String) extends Exception(message)

object Parser extends Parsers {
	override type Elem = Token
}