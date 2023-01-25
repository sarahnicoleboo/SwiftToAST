package SwiftToAST.tokenizer

import scala.util.parsing.combinator._

class TokenizerException(message: String) extends Exception(message)

object TokenizerPC extends RegexParsers {
	
	override def skipWhitespace = true
	override val whiteSpace = "[ \t\r\f]+".r	//not including \n on purpose
	
/* 	def as = "as" ^^ (_ => AsToken)
	def alpha = "alpha" ^^ (_ => AlphaToken) */
	
	def reservedWords: Parser[Token] = {
		"associatedtype" ^^ (_ => AssociatedTypeToken) | "alpha" ^^ (_ => AlphaToken) |
		"break" ^^ (_ => BreakToken) | "case" ^^ (_ => CaseToken) |
		"catch" ^^ (_ => CatchToken) | "class" ^^ (_ => ClassToken) |
		"continue" ^^ (_ => ContinueToken) | "default" ^^ (_ => DefaultToken) |
		"defer" ^^ (_ => DeferToken) | "do" ^^ (_ => DoToken) |
		"guard" ^^ (_ => GuardToken) | "else" ^^ (_ => ElseToken) |
		"enum" ^^ (_ => EnumToken) | "for" ^^ (_ => ForToken) |
		"fallthrough" ^^ (_ => FallthroughToken) | "func" ^^ (_ => FuncToken) |
		"internal" ^^ (_ => InternalToken) | "if" ^^ (_ => IfToken) |
		"import" ^^ (_ => ImportToken) | "indirect" ^^ (_ => IndirectToken) |
		"final" ^^ (_ => FinalToken) | "open" ^^ (_ => OpenToken) |
		"private" ^^ (_ => PrivateToken) | "public" ^^ (_ => PublicToken) |
		"where" ^^ (_ => WhereToken) | "while" ^^ (_ => WhileToken) |
		"let" ^^ (_ => LetToken) | "var" ^^ (_ => VarToken) |
		"protocol" ^^ (_ => ProtocolToken) | "getter" ^^ (_ => GetterToken) |
		"setter" ^^ (_ => SetterToken) | "willSet" ^^ (_ => WillSetToken) |
		"didSet" ^^ (_ => DidSetToken) | "repeat" ^^ (_ => RepeatToken) |
		"switch" ^^ (_ => SwitchToken) | "struct" ^^ (_ => StructToken) |
		"return" ^^ (_ => ReturnToken) | "throws" ^^ (_ => ThrowsToken) | 
		"throw" ^^ (_ => ThrowToken) | "rethrows" ^^ (_ => RethrowsToken) |
		"init" ^^ (_ => InitToken) | "infix" ^^ (_ => InfixToken) |
		"deinit" ^^ (_ => DeinitToken) | "assignment" ^^ (_ => AssignmentToken) |
		"extension" ^^ (_ => ExtensionToken) | "subscript" ^^ (_ => SubscriptToken) |
		"prefix" ^^ (_ => PrefixToken) | "inout" ^^ (_ => InOutToken) |
		"left" ^^ (_ => LeftToken) | "right" ^^ (_ => RightToken) |
		"none" ^^ (_ => NoneToken) | "precedencegroup" ^^ (_ => PrecedenceGroupToken) |
		"higherThan" ^^ (_ => HigherThanToken) | "lowerThan" ^^ (_ => LowerThanToken) |
		"associativity" ^^ (_ => AssociativityToken) | "as" ^^ (_ => AsToken) |
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
		"in" ^^ (_ => InToken) | "some" ^^ (_ => SomeToken) |
		"Type" ^^ (_ => TypeToken) | "precedence" ^^ (_ => PrecedenceToken) |
		"self" ^^ (_ => SelfToken) | "Self" ^^ (_ => SelfBigToken) |
		"macOSApplicationExtension" ^^ (_ => MacOSApplicationExtensionToken) | "iOSApplicationExtension" ^^ (_ => IOSApplicationExtensionToken) |
		"OSX" ^^ (_ => OSXToken) | "watchOS" ^^ (_ => WatchOSToken) |
		"tvOS" ^^ (_ => TVOSToken) | "Linux" ^^ (_ => LinuxToken) |
		"Windows" ^^ (_ => WindowsToken) | "i386" ^^ (_ => I386Token) |
		"x86_64" ^^ (_ => X86_64Token) | "arm64" ^^ (_ => Arm64Token) |
		"arm" ^^ (_ => ArmToken) | "simulator" ^^ (_ => SimulatorToken) |
		"macCatalystApplicationExtension" ^^ (_ => MacCatalystApplicationExtensionToken) | "iOS" ^^ (_ => IOSToken) |
		"macCatalyst" ^^ (_ => MacCatalystToken) | "macOS" ^^ (_ => MacOSToken) |
		"#sourceLocation" ^^ (_ => SourceLocationToken) | "file" ^^ (_ => FileToken) |
		"line" ^^ (_ => LineToken) | "#error" ^^ (_ => ErrorToken) |
		"#warning" ^^ (_ => WarningToken) | "#available" ^^ (_ => AvailableToken) |
		"#if" ^^ (_ => HashIfToken) | "#elseif" ^^ (_ => HashElseIfToken) |
		"#else" ^^ (_ => HashElseToken) | "#endif" ^^ (_ => HashEndIfToken) |
		"#fileID" ^^ (_ => HashFileIDToken) | "#filePath" ^^ (_ => HashFilePathToken) |
		"#fileLiteral" ^^ (_ => HashFileLiteralToken) | "#line" ^^ (_ => HashLineToken) |
		"#column" ^^ (_ => HashColumnToken) | "#function" ^^ (_ => HashFunctionToken) |
		"#dsohandle" ^^ (_ => HashDSOHandleToken) | "#selector" ^^ (_ => HashSelectorToken) |
		"#keyPath" ^^ (_ => HashKeyPathToken) | "#colorLiteral" ^^ (_ => HashColorLiteralToken) |
		"#file" ^^ (_ => HashFileToken) | "#imageLiteral" ^^ (_ => HashImageLiteralToken) |
		"get" ^^ (_ => GetToken) | "set" ^^ (_ => SetToken)
		}
	
	//tokenize a variable
	def variable: Parser[VariableToken] = {
		"[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => VariableToken(str) }
	}
	
/* 	def implicit_parameter: Parser[ImplicitParameterToken] = {
		"[$][0-9]+".r ^^ { str => ImplicitParameterToken(str) }
	}
	
	def property_wrapper_projection: Parser[PropertyWrapperProjectionToken] = {
		"[$][a-zA-Z0-9_]+".r ^^ { str => PropertyWrapperProjectionToken(str) }
	} */
	
	//tokenize either an implicit parameter or a property wrapper projection
	def implicit_parameter_OR_property_wrapper_projection: Parser[ImplicitParameterOrPropertyWrapperProjectionToken] = {
		"[$][a-zA-Z0-9_]+".r ^^ { str => ImplicitParameterOrPropertyWrapperProjectionToken(str) }
	}
	
	def integer_literal: Parser[Token] = {
		"[0][b][01_]*".r ^^ { str => BinaryIntegerLiteralToken(str) } |
		"[0][o][0-7_]*".r ^^ { str => OctalIntegerLiteralToken(str) } |
		"[0][x][0-9a-fA-F_]*".r ^^ { str => HexIntegerLiteralToken(str) } |
		"[0-9][0-9_]*".r ^^ { str => DecimalIntegerLiteralToken(str) }
	}
	
	def tokens: Parser[List[Token]] = {
		phrase(rep1(reservedWords | variable | implicit_parameter_OR_property_wrapper_projection | integer_literal)) ^^ { rawTokens => tokenize(rawTokens) }	//questionable
	}
	
	def tokenize(tokens: List[Token]): List[Token] = tokens
	
	
	def apply(code: String): List[Token] = {
		parse(tokens, code) match {
			case NoSuccess(message, next) => throw new TokenizerException(message)
			case Success(result, next) => result
		}
	} 
}