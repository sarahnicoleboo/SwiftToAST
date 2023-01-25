package SwiftToAST.tokenizer

import scala.util.parsing.combinator._

class TokenizerException(message: String) extends Exception(message)

object TokenizerPC extends RegexParsers {
	
	override def skipWhitespace = true
	override val whiteSpace = "[ \t\r\f]+".r	//not including \n on purpose
	
/* 	def as = "as" ^^ (_ => AsToken)
	def alpha = "alpha" ^^ (_ => AlphaToken) */
	
	def reservedWords: Parser[Token] = {
		"as" ^^ (_ => AsToken) | "alpha" ^^ (_ => AlphaToken) |
		"break" ^^ (_ => BreakToken) | "case" ^^ (_ => CaseToken) |
		"catch" ^^ (_ => CatchToken) | "class" ^^ (_ => ClassToken) |
		"continue" ^^ (_ => ContinueToken) | "default" ^^ (_ => DefaultToken) |
		"defer" ^^ (_ => DeferToken) | "do" ^^ (_ => DoToken) |
		"guard" ^^ (_ => GuardToken) | "else" ^^ (_ => ElseToken) |
		"enum" ^^ (_ => EnumToken) | "for" ^^ (_ => ForToken) |
		"fallthrough" ^^ (_ => FallthroughToken) | "func" ^^ (_ => FuncToken) |
		"in" ^^ (_ => InToken) | "if" ^^ (_ => IfToken) |
		"import" ^^ (_ => ImportToken) | "internal" ^^ (_ => InternalToken) |
		"final" ^^ (_ => FinalToken) | "open" ^^ (_ => OpenToken) |
		"private" ^^ (_ => PrivateToken) | "public" ^^ (_ => PublicToken) |
		"where" ^^ (_ => WhereToken) | "while" ^^ (_ => WhileToken) |
		"let" ^^ (_ => LetToken) | "var" ^^ (_ => VarToken) |
		"protocol" ^^ (_ => ProtocolToken) | "get" ^^ (_ => GetToken) |
		"set" ^^ (_ => SetToken) | "willSet" ^^ (_ => WillSetToken) |
		"didSet" ^^ (_ => DidSetToken) | "repeat" ^^ (_ => RepeatToken) |
		"switch" ^^ (_ => SwitchToken) | "struct" ^^ (_ => StructToken) |
		"return" ^^ (_ => ReturnToken) | "throw" ^^ (_ => ThrowToken) | 
		"throws" ^^ (_ => ThrowsToken) | "rethrows" ^^ (_ => RethrowsToken) |
		"indirect" ^^ (_ => IndirectToken) | "init" ^^ (_ => InitToken) |
		"deinint" ^^ (_ => DeinitToken) | "associatedtype" ^^ (_ => AssociatedTypeToken) |
		"extension" ^^ (_ => ExtensionToken) | "subscript" ^^ (_ => SubscriptToken) |
		"prefix" ^^ (_ => PrefixToken) | "infix" ^^ (_ => InfixToken) |
		"left" ^^ (_ => LeftToken) | "right" ^^ (_ => RightToken) |
		"none" ^^ (_ => NoneToken) | "precedencegroup" ^^ (_ => PrecedenceGroupToken) |
		"higherThan" ^^ (_ => HigherThanToken) | "lowerThan" ^^ (_ => LowerThanToken) |
		"assignment" ^^ (_ => AssignmentToken) | "associativity" ^^ (_ => AssociativityToken) |
		"postfix" ^^ (_ => PostFixToken) | "operator" ^^ (_ => OperatorToken) |
		"typealias" ^^ (_ => TypeAliasToken) | "os" ^^ (_ => OSToken) |
		"arch" ^^ (_ => ArchToken) | "swift" ^^ (_ => SwiftToken) |
		"compiler" ^^ (_ => CompilerToken) | "canImport" ^^ (_ => CanImportToken) |
		"targetEnvironment" ^^ (_ => TargetEnvironmentToken) | "convenience" ^^ (_ => ConvenienceToken) |
		"dynamic" ^^ (_ => DynamicToken) | "lazy" ^^ (_ => LazyToken) |
		"optional" ^^ (_ => OptionalToken) | "override" ^^ (_ => OverrideToken) |
		"required" ^^ (_ => RequiredToken) | "static" ^^ (_ => StaticToken) |
		"weak" ^^ (_ => WeakToken) | "unowned" ^^ (_ => UnownedToken) |
		"safe" ^^ (_ => SafeToken) | "unsafe" ^^ (_ => UnsafeToken) |
		"mutating" ^^ (_ => MutatingToken) | "nonmutating" ^^ (_ => NonmutatingToken) |
		"fileprivate" ^^ (_ => FilePrivateToken) | "is" ^^ (_ => IsToken) |
		"try" ^^ (_ => TryToken) | "super" ^^ (_ => SuperToken) |
		"Any" ^^ (_ => AnyToken) | "false" ^^ (_ => FalseToken) |
		"red" ^^ (_ => RedToken) | "blue" ^^ (_ => BlueToken) |
		"green" ^^ (_ => GreenToken) | "resourceName" ^^ (_ => ResourceNameToken) |
		"true" ^^ (_ => TrueToken) | "nil" ^^ (_ => NilToken) |
		"inout" ^^ (_ => InOutToken) | "some" ^^ (_ => SomeToken) |
		"Type" ^^ (_ => TypeToken) | "precedence" ^^ (_ => PrecedenceToken) |
		"self" ^^ (_ => SelfToken) | "Self" ^^ (_ => SelfBigToken) |
		"macOS" ^^ (_ => MacOSToken) | "iOS" ^^ (_ => IOSToken) |
		"OSX" ^^ (_ => OSXToken) | "watchOS" ^^ (_ => WatchOSToken) |
		"tvOS" ^^ (_ => TVOSToken) | "Linux" ^^ (_ => LinuxToken) |
		"Windows" ^^ (_ => WindowsToken) | "i386" ^^ (_ => I386Token) |
		"x86_64" ^^ (_ => X86_64Token) | "arm" ^^ (_ => ArmToken) |
		"arm64" ^^ (_ => Arm64Token) | "simulator" ^^ (_ => SimulatorToken) |
		"macCatalyst" ^^ (_ => MacCatalystToken) | "iOSApplicationExtension" ^^ (_ => IOSApplicationExtensionToken) |
		"macCatalystApplicationExtension" ^^ (_ => MacCatalystApplicationExtensionToken) | "macOSApplicationExtension" ^^ (_ => MacOSApplicationExtensionToken) |
		"#sourceLocation" ^^ (_ => SourceLocationToken) | "file" ^^ (_ => FileToken) |
		"line" ^^ (_ => LineToken) | "#error" ^^ (_ => ErrorToken) |
		"#warning" ^^ (_ => WarningToken) | "#available" ^^ (_ => AvailableToken) |
		"#if" ^^ (_ => HashIfToken) | "#elseif" ^^ (_ => HashElseIfToken) |
		"#else" ^^ (_ => HashElseToken) | "#endif" ^^ (_ => HashEndIfToken) |
		"#file" ^^ (_ => HashFileToken) | "#fileID" ^^ (_ => HashFileIDToken) |
		"#filePath" ^^ (_ => HashFilePathToken) | "#line" ^^ (_ => HashLineToken) |
		"#column" ^^ (_ => HashColumnToken) | "#function" ^^ (_ => HashFunctionToken) |
		"#dsohandle" ^^ (_ => HashDSOHandleToken) | "#selector" ^^ (_ => HashSelectorToken) |
		"#keyPath" ^^ (_ => HashKeyPathToken) | "#colorLiteral" ^^ (_ => HashColorLiteralToken) |
		"#fileLiteral" ^^ (_ => HashFileLiteralToken) | "#imageLiteral" ^^ (_ => HashImageLiteralToken) |
		"getter" ^^ (_ => GetterToken) | "setter" ^^ (_ => SetterToken)
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