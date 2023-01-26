package SwiftToAST.tokenizer

//import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.FlatSpec

class TokenizerTest extends FlatSpec {
	import SwiftToAST.tokenizer._
	
	"TokenizerPC" should "handle whitespace" in {
		assertResult(List(VariableToken("abc"), VariableToken("bcd"))) { TokenizerPC("abc bcd") }
	}
	
	"TokenizerPC" should "fail" in {
		assertThrows[TokenizerException] { TokenizerPC("!") }		//THIS WILL FAIL ONCE I ADD SYMBOLS LATER
	}
	
	//reserved words testing
	
	"TokenizerPC" should "handle an as token" in {
		assertResult(List(AsToken)) { TokenizerPC("as ") }
	}
	
	"TokenizerPC" should "handle an alpha token" in {
		assertResult(List(AlphaToken)) { TokenizerPC("alpha ") }
	}
	
	"TokenizerPC" should "handle an alpha token followed by an as token" in {
		assertResult(List(AlphaToken, AsToken)) { TokenizerPC("alpha as ") }
	}
	
	"TokenizerPC" should "handle a break token" in {
		assertResult(List(BreakToken)) { TokenizerPC("break ") }
	}
	
	"TokenizerPC" should "handle a case token" in {
		assertResult(List(CaseToken)) { TokenizerPC("case ") }
	}
	
	"TokenizerPC" should "handle a catch token" in {
		assertResult(List(CatchToken)) { TokenizerPC("catch ") }
	}
	
	"TokenizerPC" should "handle a class token" in {
		assertResult(List(ClassToken)) { TokenizerPC("class ") }
	}
	
	"TokenizerPC" should "handle a continue token" in {
		assertResult(List(ContinueToken)) { TokenizerPC("continue ") }
	}
	
	"TokenizerPC" should "handle a default token" in {
		assertResult(List(DefaultToken)) { TokenizerPC("default ") }
	}
	
	"TokenizerPC" should "handle a defer token" in {
		assertResult(List(DeferToken)) { TokenizerPC("defer ") }
	}
	
	"TokenizerPC" should "handle a do token" in {
		assertResult(List(DoToken)) { TokenizerPC("do ") }
	}
	
	"TokenizerPC" should "handle a guard token" in {
		assertResult(List(GuardToken)) { TokenizerPC("guard ") }
	}
	
	"TokenizerPC" should "handle an else token" in {
		assertResult(List(ElseToken)) { TokenizerPC("else ") }
	}
	
	"TokenizerPC" should "handle an enum token" in {
		assertResult(List(EnumToken)) { TokenizerPC("enum ") }
	}
	
	"TokenizerPC" should "handle a for token" in {
		assertResult(List(ForToken)) { TokenizerPC("for ") }
	}
	
	"TokenizerPC" should "handle a fallthrough token" in {
		assertResult(List(FallthroughToken)) { TokenizerPC("fallthrough ") }
	}
	
	"TokenizerPC" should "handle a func token" in {
		assertResult(List(FuncToken)) { TokenizerPC("func ") }
	}
	
	"TokenizerPC" should "handle an in token" in {
		assertResult(List(InToken)) { TokenizerPC("in ") }
	}
	
	"TokenizerPC" should "handle an if token" in {
		assertResult(List(IfToken)) { TokenizerPC("if ") }
	}
	
	"TokenizerPC" should "handle an import token" in {
		assertResult(List(ImportToken)) { TokenizerPC("import ") }
	}
	
	"TokenizerPC" should "handle an internal token" in {
		assertResult(List(InternalToken)) { TokenizerPC("internal ") }
	}
	
	"TokenizerPC" should "handle a final token" in {
		assertResult(List(FinalToken)) { TokenizerPC("final ") }
	}
	
	"TokenizerPC" should "handle an open token" in {
		assertResult(List(OpenToken)) { TokenizerPC("open ") }
	}
	
	"TokenizerPC" should "handle a private token" in {
		assertResult(List(PrivateToken)) { TokenizerPC("private ") }
	}
	
	"TokenizerPC" should "handle a public token" in {
		assertResult(List(PublicToken)) { TokenizerPC("public ") }
	}
	
	"TokenizerPC" should "handle a where token" in {
		assertResult(List(WhereToken)) { TokenizerPC("where ") }
	}
	
	"TokenizerPC" should "handle a while token" in {
		assertResult(List(WhileToken)) { TokenizerPC("while ") }
	}
	
	"TokenizerPC" should "handle a let token" in {
		assertResult(List(LetToken)) { TokenizerPC("let ") }
	}
	
	"TokenizerPC" should "handle a var token" in {
		assertResult(List(VarToken)) { TokenizerPC("var ") }
	}
	
	"TokenizerPC" should "handle a protocol token" in {
		assertResult(List(ProtocolToken)) { TokenizerPC("protocol ") }
	}
	
	"TokenizerPC" should "handle a get token" in {
		assertResult(List(GetToken)) { TokenizerPC("get ") }
	}
	
	"TokenizerPC" should "handle a set token" in {
		assertResult(List(SetToken)) { TokenizerPC("set ") }
	}
	
	"TokenizerPC" should "handle a willSet token" in {
		assertResult(List(WillSetToken)) { TokenizerPC("willSet ") }
	}
	
	"TokenizerPC" should "handle a didSet token" in {
		assertResult(List(DidSetToken)) { TokenizerPC("didSet ") }
	}
	
	"TokenizerPC" should "handle a repeat token" in {
		assertResult(List(RepeatToken)) { TokenizerPC("repeat ") }
	}
	
	"TokenizerPC" should "handle a switch token" in {
		assertResult(List(SwitchToken)) { TokenizerPC("switch ") }
	}
	
	"TokenizerPC" should "handle a struct token" in {
		assertResult(List(StructToken)) { TokenizerPC("struct ") }
	}
	
	"TokenizerPC" should "handle a return token" in {
		assertResult(List(ReturnToken)) { TokenizerPC("return ") }
	}
	
	"TokenizerPC" should "handle a throw token" in {
		assertResult(List(ThrowToken)) { TokenizerPC("throw ") }
	}
	
	"TokenizerPC" should "handle a throws token" in {
		assertResult(List(ThrowsToken)) { TokenizerPC("throws ") }
	}
	
	"TokenizerPC" should "handle a rethrows token" in {
		assertResult(List(RethrowsToken)) { TokenizerPC("rethrows ") }
	}
	
	"TokenizerPC" should "handle an indirect token" in {
		assertResult(List(IndirectToken)) { TokenizerPC("indirect ") }
	}
	
	"TokenizerPC" should "handle an init token" in {
		assertResult(List(InitToken)) { TokenizerPC("init ") }
	}
	
	"TokenizerPC" should "handle a deinit token" in {
		assertResult(List(DeinitToken)) { TokenizerPC("deinit ") }
	}
	
	"TokenizerPC" should "handle an associatedType token" in {
		assertResult(List(AssociatedTypeToken)) { TokenizerPC("associatedtype ") }
	}
	
	"TokenizerPC" should "handle an extension token" in {
		assertResult(List(ExtensionToken)) { TokenizerPC("extension ") }
	}
	
	"TokenizerPC" should "handle a subscript token" in {
		assertResult(List(SubscriptToken)) { TokenizerPC("subscript ") }
	}
	
	"TokenizerPC" should "handle a prefix token" in {
		assertResult(List(PrefixToken)) { TokenizerPC("prefix ") }
	}
	
	"TokenizerPC" should "handle an infix token" in {
		assertResult(List(InfixToken)) { TokenizerPC("infix ") }
	}
	
	"TokenizerPC" should "handle a left token" in {
		assertResult(List(LeftToken)) { TokenizerPC("left ") }
	}
	
	"TokenizerPC" should "handle a right token" in {
		assertResult(List(RightToken)) { TokenizerPC("right ") }
	}
	
	"TokenizerPC" should "handle a none token" in {
		assertResult(List(NoneToken)) { TokenizerPC("none ") }
	}
	
	"TokenizerPC" should "handle a precedencegroup token" in {
		assertResult(List(PrecedenceGroupToken)) { TokenizerPC("precedencegroup ") }
	}
	
	"TokenizerPC" should "handle a higherThan token" in {
		assertResult(List(HigherThanToken)) { TokenizerPC("higherThan ") }
	}
	
	"TokenizerPC" should "handle a lowerThan token" in {
		assertResult(List(LowerThanToken)) { TokenizerPC("lowerThan ") }
	}
	
	"TokenizerPC" should "handle an assignment token" in {
		assertResult(List(AssignmentToken)) { TokenizerPC("assignment ") }
	}
	
	"TokenizerPC" should "handle an associativity token" in {
		assertResult(List(AssociativityToken)) { TokenizerPC("associativity ") }
	}
	
	"TokenizerPC" should "handle a postfix token" in {
		assertResult(List(PostFixToken)) { TokenizerPC("postfix ") }
	}
	
	"TokenizerPC" should "handle an operator token" in {
		assertResult(List(OperatorToken)) { TokenizerPC("operator ") }
	}
	
	"TokenizerPC" should "handle a typealias token" in {
		assertResult(List(TypeAliasToken)) { TokenizerPC("typealias ") }
	}
	
	"TokenizerPC" should "handle an os token" in {
		assertResult(List(OSToken)) { TokenizerPC("os ") }
	}
	
	"TokenizerPC" should "handle an arch token" in {
		assertResult(List(ArchToken)) { TokenizerPC("arch ") }
	}
	
	"TokenizerPC" should "handle a swift token" in {
		assertResult(List(SwiftToken)) { TokenizerPC("swift ") }
	}
	
	"TokenizerPC" should "handle a compiler token" in {
		assertResult(List(CompilerToken)) { TokenizerPC("compiler ") }
	}
	
	"TokenizerPC" should "handle a canImport token" in {
		assertResult(List(CanImportToken)) { TokenizerPC("canImport ") }
	}
	
	"TokenizerPC" should "handle a targetEnvironment token" in {
		assertResult(List(TargetEnvironmentToken)) { TokenizerPC("targetEnvironment ") }
	}
	
	"TokenizerPC" should "handle a convenience token" in {
		assertResult(List(ConvenienceToken)) { TokenizerPC("convenience ") }
	}
	
	"TokenizerPC" should "handle a dynamic token" in {
		assertResult(List(DynamicToken)) { TokenizerPC("dynamic ") }
	}
	
	"TokenizerPC" should "handle a lazy token" in {
		assertResult(List(LazyToken)) { TokenizerPC("lazy ") }
	}
	
	"TokenizerPC" should "handle an optional token" in {
		assertResult(List(OptionalToken)) { TokenizerPC("optional ") }
	}
	
	"TokenizerPC" should "handle an override token" in {
		assertResult(List(OverrideToken)) { TokenizerPC("override ") }
	}
	
	"TokenizerPC" should "handle a required token" in {
		assertResult(List(RequiredToken)) { TokenizerPC("required ") }
	}
	
	"TokenizerPC" should "handle a static token" in {
		assertResult(List(StaticToken)) { TokenizerPC("static ") }
	}
	
	"TokenizerPC" should "handle a weak token" in {
		assertResult(List(WeakToken)) { TokenizerPC("weak ") }
	}
	
	"TokenizerPC" should "handle an unowned token" in {
		assertResult(List(UnownedToken)) { TokenizerPC("unowned ") }
	}
	
	"TokenizerPC" should "handle a safe token" in {
		assertResult(List(SafeToken)) { TokenizerPC("safe ") }
	}
	
	"TokenizerPC" should "handle an unsafe token" in {
		assertResult(List(UnsafeToken)) { TokenizerPC("unsafe ") }
	}
	
	"TokenizerPC" should "handle a mutating token" in {
		assertResult(List(MutatingToken)) { TokenizerPC("mutating ") }
	}
	
	"TokenizerPC" should "handle a nonmutating token" in {
		assertResult(List(NonmutatingToken)) { TokenizerPC("nonmutating ") }
	}
	
	"TokenizerPC" should "handle a fileprivate token" in {
		assertResult(List(FilePrivateToken)) { TokenizerPC("fileprivate ") }
	}
	
	"TokenizerPC" should "handle a is token" in {
		assertResult(List(IsToken)) { TokenizerPC("is ") }
	}
	
	"TokenizerPC" should "handle a try token" in {
		assertResult(List(TryToken)) { TokenizerPC("try ") }
	}
	
	"TokenizerPC" should "handle a super token" in {
		assertResult(List(SuperToken)) { TokenizerPC("super ") }
	}
	
	"TokenizerPC" should "handle an Any token" in {
		assertResult(List(AnyToken)) { TokenizerPC("Any ") }
	}
	
	"TokenizerPC" should "handle a false token" in {
		assertResult(List(FalseToken)) { TokenizerPC("false ") }
	}
	
	"TokenizerPC" should "handle a red token" in {
		assertResult(List(RedToken)) { TokenizerPC("red ") }
	}
	
	"TokenizerPC" should "handle a blue token" in {
		assertResult(List(BlueToken)) { TokenizerPC("blue ") }
	}
	
	"TokenizerPC" should "handle a green token" in {
		assertResult(List(GreenToken)) { TokenizerPC("green ") }
	}
	
	"TokenizerPC" should "handle a resourceName token" in {
		assertResult(List(ResourceNameToken)) { TokenizerPC("resourceName ") }
	}
	
	"TokenizerPC" should "handle a true token" in {
		assertResult(List(TrueToken)) { TokenizerPC("true ") }
	}
	
	"TokenizerPC" should "handle a nil token" in {
		assertResult(List(NilToken)) { TokenizerPC("nil ") }
	}
	
	"TokenizerPC" should "handle a inout token" in {
		assertResult(List(InOutToken)) { TokenizerPC("inout ") }
	}
	
	"TokenizerPC" should "handle a some token" in {
		assertResult(List(SomeToken)) { TokenizerPC("some ") }
	}
	
	"TokenizerPC" should "handle a Type token" in {
		assertResult(List(TypeToken)) { TokenizerPC("Type ") }
	}
	
	"TokenizerPC" should "handle a precedence token" in {
		assertResult(List(PrecedenceToken)) { TokenizerPC("precedence ") }
	}
	
	"TokenizerPC" should "handle a self token" in {
		assertResult(List(SelfToken)) { TokenizerPC("self ") }
	}
	
	"TokenizerPC" should "handle a Self token" in {
		assertResult(List(SelfBigToken)) { TokenizerPC("Self ") }
	}
	
	"TokenizerPC" should "handle a macOS token" in {
		assertResult(List(MacOSToken)) { TokenizerPC("macOS ") }
	}
	
	"TokenizerPC" should "handle an iOS token" in {
		assertResult(List(IOSToken)) { TokenizerPC("iOS ") }
	}
	
	"TokenizerPC" should "handle an OSX token" in {
		assertResult(List(OSXToken)) { TokenizerPC("OSX ") }
	}
	
	"TokenizerPC" should "handle a watchOS token" in {
		assertResult(List(WatchOSToken)) { TokenizerPC("watchOS ") }
	}
	
	"TokenizerPC" should "handle a tvOS token" in {
		assertResult(List(TVOSToken)) { TokenizerPC("tvOS ") }
	}
	
	"TokenizerPC" should "handle a Linux token" in {
		assertResult(List(LinuxToken)) { TokenizerPC("Linux ") }
	}
	
	"TokenizerPC" should "handle a Windows token" in {
		assertResult(List(WindowsToken)) { TokenizerPC("Windows ") }
	}
	
	"TokenizerPC" should "handle an i386 token" in {
		assertResult(List(I386Token)) { TokenizerPC("i386 ") }
	}
	
	"TokenizerPC" should "handle an x86_64 token" in {
		assertResult(List(X86_64Token)) { TokenizerPC("x86_64 ") }
	}
	
	"TokenizerPC" should "handle an arm token" in {
		assertResult(List(ArmToken)) { TokenizerPC("arm ") }
	}
	
	"TokenizerPC" should "handle an arm64 token" in {
		assertResult(List(Arm64Token)) { TokenizerPC("arm64 ") }
	}
	
	"TokenizerPC" should "handle a simulator token" in {
		assertResult(List(SimulatorToken)) { TokenizerPC("simulator ") }
	}
	
	"TokenizerPC" should "handle a macCatalyst token" in {
		assertResult(List(MacCatalystToken)) { TokenizerPC("macCatalyst ") }
	}

	"TokenizerPC" should "handle an iOSApplicationExtension token" in {
		assertResult(List(IOSApplicationExtensionToken)) { TokenizerPC("iOSApplicationExtension ") }
	}
	
	"TokenizerPC" should "handle a macCatalystApplicationExtension token" in {
		assertResult(List(MacCatalystApplicationExtensionToken)) { TokenizerPC("macCatalystApplicationExtension ") }
	}
	
	"TokenizerPC" should "handle a macOSApplicationExtension token" in {
		assertResult(List(MacOSApplicationExtensionToken)) { TokenizerPC("macOSApplicationExtension ") }
	}
	
	"TokenizerPC" should "handle a #sourceLocation token" in {
		assertResult(List(SourceLocationToken)) { TokenizerPC("#sourceLocation ") }
	}
	
	"TokenizerPC" should "handle a file token" in {
		assertResult(List(FileToken)) { TokenizerPC("file ") }
	}
	
	"TokenizerPC" should "handle a line token" in {
		assertResult(List(LineToken)) { TokenizerPC("line ") }
	}
	
	"TokenizerPC" should "handle a #error token" in {
		assertResult(List(ErrorToken)) { TokenizerPC("#error ") }
	}
	
	"TokenizerPC" should "handle a #warning token" in {
		assertResult(List(WarningToken)) { TokenizerPC("#warning ") }
	}
	
	"TokenizerPC" should "handle a #available token" in {
		assertResult(List(AvailableToken)) { TokenizerPC("#available ") }
	}
	
	"TokenizerPC" should "handle a #if token" in {
		assertResult(List(HashIfToken)) { TokenizerPC("#if ") }
	}
	
	"TokenizerPC" should "handle a #elseif token" in {
		assertResult(List(HashElseIfToken)) { TokenizerPC("#elseif ") }
	}
	
	"TokenizerPC" should "handle a #else token" in {
		assertResult(List(HashElseToken)) { TokenizerPC("#else ") }
	}
	
	"TokenizerPC" should "handle a #endif token" in {
		assertResult(List(HashEndIfToken)) { TokenizerPC("#endif ") }
	}
	
	"TokenizerPC" should "handle a #file token" in {
		assertResult(List(HashFileToken)) { TokenizerPC("#file ") }
	}
	
	"TokenizerPC" should "handle a #fileID token" in {
		assertResult(List(HashFileIDToken)) { TokenizerPC("#fileID ") }
	}
	
	"TokenizerPC" should "handle a #filePath token" in {
		assertResult(List(HashFilePathToken)) { TokenizerPC("#filePath ") }
	}
	
	"TokenizerPC" should "handle a #line token" in {
		assertResult(List(HashLineToken)) { TokenizerPC("#line ") }
	}
	
	"TokenizerPC" should "handle a #column token" in {
		assertResult(List(HashColumnToken)) { TokenizerPC("#column ") }
	}
	
	"TokenizerPC" should "handle a #function token" in {
		assertResult(List(HashFunctionToken)) { TokenizerPC("#function ") }
	}
	
	"TokenizerPC" should "handle a #dsohandle token" in {
		assertResult(List(HashDSOHandleToken)) { TokenizerPC("#dsohandle ") }
	}
	
	"TokenizerPC" should "handle a #selector token" in {
		assertResult(List(HashSelectorToken)) { TokenizerPC("#selector ") }
	}
	
	"TokenizerPC" should "handle a #keyPath token" in {
		assertResult(List(HashKeyPathToken)) { TokenizerPC("#keyPath ") }
	}
	
	"TokenizerPC" should "handle a #colorLiteral token" in {
		assertResult(List(HashColorLiteralToken)) { TokenizerPC("#colorLiteral ") }
	}
	
	"TokenizerPC" should "handle a #fileLiteral token" in {
		assertResult(List(HashFileLiteralToken)) { TokenizerPC("#fileLiteral ") }
	}
	
	"TokenizerPC" should "handle a #imageLiteral token" in {
		assertResult(List(HashImageLiteralToken)) { TokenizerPC("#imageLiteral ") }
	}
	
	"TokenizerPC" should "handle a getter token" in {
		assertResult(List(GetterToken)) { TokenizerPC("getter ") }
	}
	
	"TokenizerPC" should "handle a setter token" in {
		assertResult(List(SetterToken)) { TokenizerPC("setter ") }
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
	
	"TokenizerPC" should "handle this problem" in {
		assertResult(List(VariableToken("var_thing")))  { TokenizerPC("var_thing") }
	}
	
	"TokenizerPC" should "handle this other problem" in {
		assertResult(List(VarToken, MinusToken, VarToken))  { TokenizerPC("var-var ") }
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
	
	
	//number testing
	
	//decimal integers
	"TokenizerPC" should "handle a decimal integer literal with one digit" in {
		assertResult(List(DecimalIntegerLiteralToken("2"))) { TokenizerPC("2") }
	}
	
	"TokenizerPC" should "handle a decimal integer literal with multiple digits" in {
		assertResult(List(DecimalIntegerLiteralToken("463456"))) { TokenizerPC("463456") }
	}
	
	"TokenizerPC" should "handle a decimal integer literal with multiple digits and delimeters" in {
		assertResult(List(DecimalIntegerLiteralToken("123_345"))) { TokenizerPC("123_345") }
	}
	
	//binary integers
	"TokenizerPC" should "handle a binary integer literal with one digit" in {
		assertResult(List(BinaryIntegerLiteralToken("0b1"))) { TokenizerPC("0b1") }
	}
	
	"TokenizerPC" should "handle a binary integer literal with multiple digits" in {
		assertResult(List(BinaryIntegerLiteralToken("0b1011"))) { TokenizerPC("0b1011") }
	}
	
	"TokenizerPC" should "handle a binary integer literal with multiple digits and delimeters" in {
		assertResult(List(BinaryIntegerLiteralToken("0b1_011"))) { TokenizerPC("0b1_011") }
	}
	
	//octal integers
	"TokenizerPC" should "handle an octal integer literal with one digit" in {
		assertResult(List(OctalIntegerLiteralToken("0o5"))) { TokenizerPC("0o5") }
	}
	
	"TokenizerPC" should "handle an octal integer literal with multiple digits" in {
		assertResult(List(OctalIntegerLiteralToken("0o347"))) { TokenizerPC("0o347") }
	}
	
	"TokenizerPC" should "handle an octal integer literal with multiple digits and delimeters" in {
		assertResult(List(OctalIntegerLiteralToken("0o34_7"))) { TokenizerPC("0o34_7") }
	}
	
	//hex integers
	"TokenizerPC" should "handle a hex integer literal with one digit" in {
		assertResult(List(HexIntegerLiteralToken("0x8"))) { TokenizerPC("0x8") }
	}
	
	"TokenizerPC" should "handle a hex integer literal with multiple digits" in {
		assertResult(List(HexIntegerLiteralToken("0xA4b3"))) { TokenizerPC("0xA4b3") }
	}
	
	"TokenizerPC" should "handle a hex integer literal with multiple digits and delimeters" in {
		assertResult(List(HexIntegerLiteralToken("0xF_A4b3"))) { TokenizerPC("0xF_A4b3") }
	}
	
	//decimal floats
	"TokenizerPC" should "handle a decimal float literal with a decimal point" in {
		assertResult(List(FloatDecimalLiteralToken("12.3"))) { TokenizerPC("12.3") }
	}
	
	"TokenizerPC" should "handle a decimal float literal with a decimal point and delimeters" in {
		assertResult(List(FloatDecimalLiteralToken("12.3_5"))) { TokenizerPC("12.3_5") }
	}
	
	"TokenizerPC" should "handle a decimal float literal with a floating point e" in {
		assertResult(List(FloatDecimalLiteralToken("12e3"))) { TokenizerPC("12e3") }
	}
	
	"TokenizerPC" should "handle a decimal float literal with a floating point e and delimeters" in {
		assertResult(List(FloatDecimalLiteralToken("12e3_5"))) { TokenizerPC("12e3_5") }
	}
}
