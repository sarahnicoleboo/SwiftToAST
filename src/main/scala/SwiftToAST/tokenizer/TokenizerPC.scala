package SwiftToAST.tokenizer

import scala.util.parsing.combinator._

class TokenizerException(message: String) extends Exception(message)

object TokenizerPC extends RegexParsers {
	
	def reservedWords: Parser[Token] = {
		"associatedtype(?=[\\s\\W]+)".r ^^ (_ => AssociatedTypeToken) | "alpha(?=[\\s\\W]+)".r ^^ (_ => AlphaToken) |
		"break(?=[\\s\\W]+)".r ^^ (_ => BreakToken) | "case(?=[\\s\\W]+)".r ^^ (_ => CaseToken) |
		"catch(?=[\\s\\W]+)".r ^^ (_ => CatchToken) | "class(?=[\\s\\W]+)".r ^^ (_ => ClassToken) |
		"continue(?=[\\s\\W]+)".r ^^ (_ => ContinueToken) | "default(?=[\\s\\W]+)".r ^^ (_ => DefaultToken) |
		"defer(?=[\\s\\W]+)".r ^^ (_ => DeferToken) | "do(?=[\\s\\W]+)".r ^^ (_ => DoToken) |
		"guard(?=[\\s\\W]+)".r ^^ (_ => GuardToken) | "else(?=[\\s\\W]+)".r ^^ (_ => ElseToken) |
		"enum(?=[\\s\\W]+)".r ^^ (_ => EnumToken) | "for(?=[\\s\\W]+)".r ^^ (_ => ForToken) |
		"fallthrough(?=[\\s\\W]+)".r ^^ (_ => FallthroughToken) | "func(?=[\\s\\W]+)".r ^^ (_ => FuncToken) |
		"internal(?=[\\s\\W]+)".r ^^ (_ => InternalToken) | "if(?=[\\s\\W]+)".r ^^ (_ => IfToken) |
		"import(?=[\\s\\W]+)".r ^^ (_ => ImportToken) | "indirect(?=[\\s\\W]+)".r ^^ (_ => IndirectToken) |
		"final(?=[\\s\\W]+)".r ^^ (_ => FinalToken) | "open(?=[\\s\\W]+)".r ^^ (_ => OpenToken) |
		"private(?=[\\s\\W]+)".r ^^ (_ => PrivateToken) | "public(?=[\\s\\W]+)".r ^^ (_ => PublicToken) |
		"where(?=[\\s\\W]+)".r ^^ (_ => WhereToken) | "while(?=[\\s\\W]+)".r ^^ (_ => WhileToken) |
		"let(?=[\\s\\W]+)".r ^^ (_ => LetToken) | "var(?=[\\s\\W]+)".r ^^ (_ => VarToken) |
		"protocol(?=[\\s\\W]+)".r ^^ (_ => ProtocolToken) | "getter(?=[\\s\\W]+)".r ^^ (_ => GetterToken) |
		"setter(?=[\\s\\W]+)".r ^^ (_ => SetterToken) | "willSet(?=[\\s\\W]+)".r ^^ (_ => WillSetToken) |
		"didSet(?=[\\s\\W]+)".r ^^ (_ => DidSetToken) | "repeat(?=[\\s\\W]+)".r ^^ (_ => RepeatToken) |
		"switch(?=[\\s\\W]+)".r ^^ (_ => SwitchToken) | "struct(?=[\\s\\W]+)".r ^^ (_ => StructToken) |
		"return(?=[\\s\\W]+)".r ^^ (_ => ReturnToken) | "throws(?=[\\s\\W]+)".r ^^ (_ => ThrowsToken) | 
		"throw(?=[\\s\\W]+)".r ^^ (_ => ThrowToken) | "rethrows(?=[\\s\\W]+)".r ^^ (_ => RethrowsToken) |
		"init(?=[\\s\\W]+)".r ^^ (_ => InitToken) | "infix(?=[\\s\\W]+)".r ^^ (_ => InfixToken) |
		"deinit(?=[\\s\\W]+)".r ^^ (_ => DeinitToken) | "assignment(?=[\\s\\W]+)".r ^^ (_ => AssignmentToken) |
		"extension(?=[\\s\\W]+)".r ^^ (_ => ExtensionToken) | "subscript(?=[\\s\\W]+)".r ^^ (_ => SubscriptToken) |
		"prefix(?=[\\s\\W]+)".r ^^ (_ => PrefixToken) | "inout(?=[\\s\\W]+)".r ^^ (_ => InOutToken) |
		"left(?=[\\s\\W]+)".r ^^ (_ => LeftToken) | "right(?=[\\s\\W]+)".r ^^ (_ => RightToken) |
		"none(?=[\\s\\W]+)".r ^^ (_ => NoneToken) | "precedencegroup(?=[\\s\\W]+)".r ^^ (_ => PrecedenceGroupToken) |
		"higherThan(?=[\\s\\W]+)".r ^^ (_ => HigherThanToken) | "lowerThan(?=[\\s\\W]+)".r ^^ (_ => LowerThanToken) |
		"associativity(?=[\\s\\W]+)".r ^^ (_ => AssociativityToken) | "as(?=[\\s\\W]+)".r ^^ (_ => AsToken) |
		"postfix(?=[\\s\\W]+)".r ^^ (_ => PostFixToken) | "operator(?=[\\s\\W]+)".r ^^ (_ => OperatorToken) |
		"typealias(?=[\\s\\W]+)".r ^^ (_ => TypeAliasToken) | "os(?=[\\s\\W]+)".r ^^ (_ => OSToken) |
		"arch(?=[\\s\\W]+)".r ^^ (_ => ArchToken) | "swift(?=[\\s\\W]+)".r ^^ (_ => SwiftToken) |
		"compiler(?=[\\s\\W]+)".r ^^ (_ => CompilerToken) | "canImport(?=[\\s\\W]+)".r ^^ (_ => CanImportToken) |
		"targetEnvironment(?=[\\s\\W]+)".r ^^ (_ => TargetEnvironmentToken) | "convenience(?=[\\s\\W]+)".r ^^ (_ => ConvenienceToken) |
		"dynamic(?=[\\s\\W]+)".r ^^ (_ => DynamicToken) | "lazy(?=[\\s\\W]+)".r ^^ (_ => LazyToken) |
		"optional(?=[\\s\\W]+)".r ^^ (_ => OptionalToken) | "override(?=[\\s\\W]+)".r ^^ (_ => OverrideToken) |
		"required(?=[\\s\\W]+)".r ^^ (_ => RequiredToken) | "static(?=[\\s\\W]+)".r ^^ (_ => StaticToken) |
		"weak(?=[\\s\\W]+)".r ^^ (_ => WeakToken) | "unowned(?=[\\s\\W]+)".r ^^ (_ => UnownedToken) |
		"safe(?=[\\s\\W]+)".r ^^ (_ => SafeToken) | "unsafe(?=[\\s\\W]+)".r ^^ (_ => UnsafeToken) |
		"mutating(?=[\\s\\W]+)".r ^^ (_ => MutatingToken) | "nonmutating(?=[\\s\\W]+)".r ^^ (_ => NonmutatingToken) |
		"fileprivate(?=[\\s\\W]+)".r ^^ (_ => FilePrivateToken) | "is(?=[\\s\\W]+)".r ^^ (_ => IsToken) |
		"try(?=[\\s\\W]+)".r ^^ (_ => TryToken) | "super(?=[\\s\\W]+)".r ^^ (_ => SuperToken) |
		"Any(?=[\\s\\W]+)".r ^^ (_ => AnyToken) | "false(?=[\\s\\W]+)".r ^^ (_ => FalseToken) |
		"red(?=[\\s\\W]+)".r ^^ (_ => RedToken) | "blue(?=[\\s\\W]+)".r ^^ (_ => BlueToken) |
		"green(?=[\\s\\W]+)".r ^^ (_ => GreenToken) | "resourceName(?=[\\s\\W]+)".r ^^ (_ => ResourceNameToken) |
		"true(?=[\\s\\W]+)".r ^^ (_ => TrueToken) | "nil(?=[\\s\\W]+)".r ^^ (_ => NilToken) |
		"in(?=[\\s\\W]+)".r ^^ (_ => InToken) | "some(?=[\\s\\W]+)".r ^^ (_ => SomeToken) |
		"Type(?=[\\s\\W]+)".r ^^ (_ => TypeToken) | "precedence(?=[\\s\\W]+)".r ^^ (_ => PrecedenceToken) |
		"self(?=[\\s\\W]+)".r ^^ (_ => SelfToken) | "Self(?=[\\s\\W]+)".r ^^ (_ => SelfBigToken) |
		"macOSApplicationExtension(?=[\\s\\W]+)".r ^^ (_ => MacOSApplicationExtensionToken) | "iOSApplicationExtension(?=[\\s\\W]+)".r ^^ (_ => IOSApplicationExtensionToken) |
		"OSX(?=[\\s\\W]+)".r ^^ (_ => OSXToken) | "watchOS(?=[\\s\\W]+)".r ^^ (_ => WatchOSToken) |
		"tvOS(?=[\\s\\W]+)".r ^^ (_ => TVOSToken) | "Linux(?=[\\s\\W]+)".r ^^ (_ => LinuxToken) |
		"Windows(?=[\\s\\W]+)".r ^^ (_ => WindowsToken) | "i386(?=[\\s\\W]+)".r ^^ (_ => I386Token) |
		"x86_64(?=[\\s\\W]+)".r ^^ (_ => X86_64Token) | "arm64(?=[\\s\\W]+)".r ^^ (_ => Arm64Token) |
		"arm(?=[\\s\\W]+)".r ^^ (_ => ArmToken) | "simulator(?=[\\s\\W]+)".r ^^ (_ => SimulatorToken) |
		"macCatalystApplicationExtension(?=[\\s\\W]+)".r ^^ (_ => MacCatalystApplicationExtensionToken) | "iOS(?=[\\s\\W]+)".r ^^ (_ => IOSToken) |
		"macCatalyst(?=[\\s\\W]+)".r ^^ (_ => MacCatalystToken) | "macOS(?=[\\s\\W]+)".r ^^ (_ => MacOSToken) |
		"#sourceLocation(?=[\\s\\W]+)".r ^^ (_ => SourceLocationToken) | "file(?=[\\s\\W]+)".r ^^ (_ => FileToken) |
		"line(?=[\\s\\W]+)".r ^^ (_ => LineToken) | "#error(?=[\\s\\W]+)".r ^^ (_ => ErrorToken) |
		"#warning(?=[\\s\\W]+)".r ^^ (_ => WarningToken) | "#available(?=[\\s\\W]+)".r ^^ (_ => AvailableToken) |
		"#if(?=[\\s\\W]+)".r ^^ (_ => HashIfToken) | "#elseif(?=[\\s\\W]+)".r ^^ (_ => HashElseIfToken) |
		"#else(?=[\\s\\W]+)".r ^^ (_ => HashElseToken) | "#endif(?=[\\s\\W]+)".r ^^ (_ => HashEndIfToken) |
		"#fileID(?=[\\s\\W]+)".r ^^ (_ => HashFileIDToken) | "#filePath(?=[\\s\\W]+)".r ^^ (_ => HashFilePathToken) |
		"#fileLiteral(?=[\\s\\W]+)".r ^^ (_ => HashFileLiteralToken) | "#line(?=[\\s\\W]+)".r ^^ (_ => HashLineToken) |
		"#column(?=[\\s\\W]+)".r ^^ (_ => HashColumnToken) | "#function(?=[\\s\\W]+)".r ^^ (_ => HashFunctionToken) |
		"#dsohandle(?=[\\s\\W]+)".r ^^ (_ => HashDSOHandleToken) | "#selector(?=[\\s\\W]+)".r ^^ (_ => HashSelectorToken) |
		"#keyPath(?=[\\s\\W]+)".r ^^ (_ => HashKeyPathToken) | "#colorLiteral(?=[\\s\\W]+)".r ^^ (_ => HashColorLiteralToken) |
		"#file(?=[\\s\\W]+)".r ^^ (_ => HashFileToken) | "#imageLiteral(?=[\\s\\W]+)".r ^^ (_ => HashImageLiteralToken) |
		"get(?=[\\s\\W]+)".r ^^ (_ => GetToken) | "set(?=[\\s\\W]+)".r ^^ (_ => SetToken)
	}

	//example:
	//	as a variable			: var _ = "hi"
	//	as an underscore token	: case(_ , 1) 
	//verdict: going to tokenize all single underscore characters as underscore tokens and disambiguate in the parser.
	def underscore = { "_(?=[\\s\\W]+)".r ^^ (_ => UnderscoreToken) }
	
	//tokenize a variable
	def variable: Parser[VariableToken] = {
		"[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => VariableToken(str) }
	}
	
	//tokenize either an implicit parameter or a property wrapper projection
	def implicit_parameter_OR_property_wrapper_projection = {
		"[$][a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => PropertyWrapperProjectionToken(str) } |
		"[$][0-9]+[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => PropertyWrapperProjectionToken(str) } |
		"[$][0-9]+".r ^^ { str => ImplicitParameterOrPropertyWrapperProjectionToken(str) }
		//"[$][a-zA-Z0-9_]+".r ^^ { str => ImplicitParameterOrPropertyWrapperProjectionToken(str) }
	}
	
	//not finished, went on side quest
	def float_literal = {
		"[0][x][0-9a-fA-F][0-9a-fA-F_]*[.][0-9a-fA-F][0-9a-fA-F_]*".r ^^ { str => FloatHexLiteralToken(str) } |
		"[0][x][0-9a-fA-F][0-9a-fA-F_]*[Pp][+-]?[0-9a-fA-F][0-9a-fA-F_]*".r ^^ { str => FloatHexLiteralToken(str) } |
		"[0-9][0-9_]*[.][0-9][0-9_]*".r ^^ { str => FloatDecimalLiteralToken(str) } |
		"[0-9][0-9_]*[Ee][0-9][0-9_]*".r ^^ { str => FloatDecimalLiteralToken(str) }
	}
	
	def integer_literal = {
		"[0][b][01][01_]*".r ^^ { str => BinaryIntegerLiteralToken(str) } |
		"[0][o][0-7][0-7_]*".r ^^ { str => OctalIntegerLiteralToken(str) } |
		"[0][x][0-9a-fA-F][0-9a-fA-F_]*".r ^^ { str => HexIntegerLiteralToken(str) } |
		"[0-9][0-9_]*".r ^^ { str => DecimalIntegerLiteralToken(str) }
	}
	
	def comments = {
		"[/][/].*[\n]".r ^^ { str => SingleLineCommentToken(str) } |
		"[/][*](.|\n)*?[*][/]".r ^^ { str => MultiLineCommentToken(str) }
	}
	
	def strings: Parser[StringLiteralToken] = {
		"""["\\]["\\]["\\](.|\n)*[\\"]["\\]["\\]""".r ^^ { str => StringLiteralToken(str) } |
		"""["\\].*[\\"]""".r ^^ { str => StringLiteralToken(str) }
		//""""((?:[^"\\]|\\[\\"ntbrf])+)"""".r ^^ { str => StringLiteralToken(str) }
	}
	
	def reservedSymbols: Parser[Token] = {
		"." ^^ (_ => DotToken) | "{" ^^ (_ => LeftCurlyToken) |
		"(" ^^ (_ => LeftParenToken) | "[" ^^ (_ => LeftBracketToken) |
		"}" ^^ (_ => RightCurlyToken) | ")" ^^ (_ => RightParenToken) |
		"]" ^^ (_ => RightBracketToken) | "," ^^ (_ => CommaToken) |
		":" ^^ (_ => ColonToken) | ";" ^^ (_ => SemicolonToken) |
		"<" ^^ (_ => LessThanToken) | ">" ^^ (_ => GreaterThanToken) |
		"!" ^^ (_ => ExclamationToken) | "?" ^^ (_ => QuestionToken) |
		"@" ^^ (_ => AtToken) | "&" ^^ (_ => AndToken) |
		"-" ^^ (_ => MinusToken) | "=" ^^ (_ => EqualToken) |
		"|" ^^ (_ => OrToken) | "/" ^^ (_ => DivisionToken) |
		"+" ^^ (_ => AdditionToken) | "*" ^^ (_ => MultiplicationToken) |
		"%" ^^ (_ => ModToken) | "^" ^^ (_ => CaretToken) |
		"~" ^^ (_ => TildeToken) | "#" ^^ (_ => HashToken) |
		"`" ^^ (_ => BackTickToken) | "\\" ^^ (_ => DoubleBackSlashToken) //|
		//"_" ^^ (_ => UnderscoreToken)
	}
	
	def tokens: Parser[List[Token]] = {
		phrase(rep(reservedWords | underscore | variable | implicit_parameter_OR_property_wrapper_projection
				| float_literal | integer_literal | comments | strings | reservedSymbols))
	}
	
	
	def apply(code: String): List[Token] = {
		parse(tokens, code) match {
			case NoSuccess(message, next) => throw new TokenizerException(message)
			case Success(result, next) => result
		}
	} 
}