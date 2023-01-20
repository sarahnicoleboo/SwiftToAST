package SwiftToAST.tokenizer

class TokenizerException(message: String) extends Exception(message)

object Tokenizer {
	val symbols: Seq[(String, Token)] =
		Seq(
			(".", DotToken),
			("{", LeftCurlyToken),
			("(", LeftParenToken),
			("[", LeftBracketToken),
			("}", RightCurlyToken),
			(")", RightParenToken),
			("]", RightBracketToken),
			(",", CommaToken),
			(":", ColonToken),
			(";", SemicolonToken),
			("<", LessThanToken),
			(">", GreaterThanToken),
			("_", UnderscoreToken),
			("!", ExclamationToken),
			("?", QuestionToken),
			("@", AtToken),
			("&", AndToken),
			("-", MinusToken),
			("=", EqualToken),
			("|", OrToken),
			("/", DivisionToken),
			("+", AdditionToken),
			("*", MultiplicationToken),
			("%", ModToken),
			("^", CaretToken),
			("~", TildeToken),
			("#", HashToken),
			("`", BackTickToken),
			("\\", DoubleBackSlashToken)
		)
	
	val reservedWords: Seq[(String, Token)] =
		Seq(
			("as ", AsToken),
			("alpha", AlphaToken),
			("break", BreakToken),
			("case", CaseToken),
			("catch", CatchToken),
			("class", ClassToken),
			("continue", ContinueToken),
			("default", DefaultToken),
			("defer", DeferToken),
			("do", DoToken),
			("guard", GuardToken),
			("else", ElseToken),
			("enum", EnumToken),
			("for", ForToken),
			("fallthrough", FallthroughToken),
			("func", FuncToken),
			("in", InToken),
			("if", IfToken),
			("import", ImportToken),
			("internal", InternalToken),
			("final", FinalToken),
			("open", OpenToken),
			("private", PrivateToken),
			("public", PublicToken),
			("where", WhereToken),
			("while", WhileToken),
			("let", LetToken),
			("var", VarToken),
			("protocol", ProtocolToken),
			("get", GetToken),
			("set", SetToken),
			("willSet", WillSetToken),
			("didSet", DidSetToken),
			("repeat", RepeatToken),
			("switch", SwitchToken),
			("struct", StructToken),
			("return", ReturnToken),
			("throw", ThrowToken),
			("throws", ThrowsToken),
			("rethrows", RethrowsToken),
			("indirect", IndirectToken),
			("init", InitToken),
			("deinit", DeinitToken),
			("associatedtype", AssociatedTypeToken),
			("extension", ExtensionToken),
			("subscript", SubscriptToken),
			("prefix", PrefixToken),
			("infix", InfixToken),
			("left", LeftToken),
			("right", RightToken),
			("none", NoneToken),
			("precedencegroup", PrecedenceGroupToken),
			("higherThan", HigherThanToken),
			("lowerThan", LowerThanToken),
			("assignment", AssignmentToken),
			("associativity", AssociativityToken),
			("postfix", PostFixToken),
			("operator", OperatorToken),
			("typealias", TypeAliasToken),
			("os", OSToken),
			("arch", ArchToken),
			("swift", SwiftToken),
			("compiler", CompilerToken),
			("canImport", CanImportToken),
			("targetEnvironment", TargetEnvironmentToken),
			("convenience", ConvenienceToken),
			("dynamic", DynamicToken),
			("lazy", LazyToken),
			("optional", OptionalToken),
			("override", OverrideToken),
			("required", RequiredToken),
			("static", StaticToken),
			("weak", WeakToken),
			("unowned", UnownedToken),
			("safe", SafeToken),
			("unsafe", UnsafeToken),
			("mutating", MutatingToken),
			("nonmutating", NonmutatingToken),
			("fileprivate", FilePrivateToken),
			("is", IsToken),
			("try", TryToken),
			("super", SuperToken),
			("Any", AnyToken),
			("false", FalseToken),
			("red", RedToken),
			("blue", BlueToken),
			("green", GreenToken),
			("resourceName", ResourceNameToken),
			("true", TrueToken),
			("nil", NilToken),
			("inout", InOutToken),
			("some", SomeToken),
			("Type", TypeToken),
			("precedence", PrecedenceToken),
			("self", SelfToken),
			("Self", SelfBigToken),
			("macOS", MacOSToken),
			("iOS", IOSToken),
			("OSX", OSXToken),
			("watchOS", WatchOSToken),
			("tvOS", TVOSToken),
			("Linux", LinuxToken),
			("Windows", WindowsToken),
			("i386", I386Token),
			("x86_64", X86_64Token),
			("arm", ArmToken),
			("arm64", Arm64Token),
			("simulator", SimulatorToken),
			("macCatalyst", MacCatalystToken),
			("iOSApplicationExtension", IOSApplicationExtensionToken),
			("macCatalystApplicationExtension", MacCatalystApplicationExtensionToken),
			("macOSApplicationExtension", MacOSApplicationExtensionToken),
			("#sourceLocation", SourceLocationToken),
			("file", FileToken),
			("line", LineToken),
			("#error", ErrorToken),
			("#warning", WarningToken),
			("#available", AvailableToken),
			("#if", HashIfToken),
			("#elseif", HashElseIfToken),
			("#else", HashElseToken),
			("#endif", HashEndIfToken),
			("#file", HashFileToken),
			("#fileID", HashFileIDToken),
			("#filePath", HashFilePathToken),
			("#line", HashLineToken),
			("#column", HashColumnToken),
			("#function", HashFunctionToken),
			("#dsohandle", HashDSOHandleToken),
			("#selector", HashSelectorToken),
			("#keyPath", HashKeyPathToken),
			("#colorLiteral", HashColorLiteralToken),
			("#fileLiteral", HashFileLiteralToken),
			("#imageLiteral", HashImageLiteralToken),
			("getter", GetterToken),
			("setter", SetterToken)
		)
	
	def tokenize(input: String): Seq[Token] = {
		new Tokenizer(input).tokenize
	}
}

class Tokenizer(val input: String) {

	def getCharacter(position: Int): Option[Char] = {
		if (position >= 0 && position < input.length) {
			Some(input.charAt(position))
		} else {
			None
		}
	}

	def getCharacters(position: Int, predicate: Char => Boolean): List[Char] = {
		def loop(currentPosition: Int, accum: List[Char]): List[Char] = {
			getCharacter(currentPosition).filter(predicate).map(c =>
				loop(currentPosition + 1, c :: accum)).getOrElse(accum.reverse)
		}
		loop(position, List())
	}

	def skipWhitespace(position: Int): Int = {
		position + getCharacters(position, Character.isWhitespace _).size
	}
	
	def tokenizeSymbol(position: Int): Option[(Token, Int)] = {
		Tokenizer.symbols.find(pair => input.startsWith(pair._1, position)).map(pair =>
			(pair._2, position + pair._1.length))
	}
	
	def tokenizeReservedWord(position: Int): Option[(Token, Int)] = {
		//should match if the word exists but return None if it doesn't
		//gonna use same idea as tokenizeSymbol
		Tokenizer.reservedWords.find(pair => input.startsWith(pair._1, position)).map(pair =>
			(pair._2, position + pair._1.length))
	}
	
	//still need: tokenizeVariable, tokenizeNumber?, tokenizeString?
	
	def tokenizeSingle(position: Int): (Token, Int) = {
		//here is where i'll call my individual tokenizeBLAH methods and then throw an exception
		//reservedWords exact match (detecting white space after)
		//numbers (look for either starting with num or starting with 0b, 0x, 0o prefixes
		//variables starts with _, $, or A-Za-z
		//comments exact match on // blahblahblah \n or /* blah blah blah */
		//exact match "blah" or """blah blah""" or #"blah\nblah" or #"""blah blah"""
		//symbols starts with any reserved symbols
		tokenizeReservedWord(position).getOrElse(throw new TokenizerException("Cannot tokenize at this positon: " + position))
	}

	def tokenize(position: Int): Seq[Token] = {
		def loop(currentPosition: Int, accum: List[Token]): List[Token] = {
			val afterWS = skipWhitespace(currentPosition)
			if (afterWS >= input.length) {
				accum.reverse
			} else {
				val (token, nextPosition) = tokenizeSingle(afterWS)
				loop(nextPosition, token :: accum)
			}
		}
		loop(position, List())
	}

	def tokenize: Seq[Token] = tokenize(0)
}