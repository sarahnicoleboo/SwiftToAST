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
	
	"TokenizerPC" should "handle a break token" in {
		assertResult(List(BreakToken)) { TokenizerPC("break") }
	}
	
	"TokenizerPC" should "handle a case token" in {
		assertResult(List(CaseToken)) { TokenizerPC("case") }
	}
	
	"TokenizerPC" should "handle a catch token" in {
		assertResult(List(CatchToken)) { TokenizerPC("catch") }
	}
	
	"TokenizerPC" should "handle a class token" in {
		assertResult(List(ClassToken)) { TokenizerPC("class") }
	}
	
	"TokenizerPC" should "handle a continue token" in {
		assertResult(List(ContinueToken)) { TokenizerPC("continue") }
	}
	
	"TokenizerPC" should "handle a default token" in {
		assertResult(List(DefaultToken)) { TokenizerPC("default") }
	}
	
	"TokenizerPC" should "handle a defer token" in {
		assertResult(List(DeferToken)) { TokenizerPC("defer") }
	}
	
	"TokenizerPC" should "handle a do token" in {
		assertResult(List(DoToken)) { TokenizerPC("do") }
	}
	
	"TokenizerPC" should "handle a guard token" in {
		assertResult(List(GuardToken)) { TokenizerPC("guard") }
	}
	
	"TokenizerPC" should "handle an else token" in {
		assertResult(List(ElseToken)) { TokenizerPC("else") }
	}
	
	"TokenizerPC" should "handle an enum token" in {
		assertResult(List(EnumToken)) { TokenizerPC("enum") }
	}
	
	"TokenizerPC" should "handle a for token" in {
		assertResult(List(ForToken)) { TokenizerPC("for") }
	}
	
	"TokenizerPC" should "handle a fallthrough token" in {
		assertResult(List(FallthroughToken)) { TokenizerPC("fallthrough") }
	}
	
	"TokenizerPC" should "handle a func token" in {
		assertResult(List(FuncToken)) { TokenizerPC("func") }
	}
	
	"TokenizerPC" should "handle an in token" in {
		assertResult(List(InToken)) { TokenizerPC("in") }
	}
	
	"TokenizerPC" should "handle an if token" in {
		assertResult(List(IfToken)) { TokenizerPC("if") }
	}
	
	"TokenizerPC" should "handle an import token" in {
		assertResult(List(ImportToken)) { TokenizerPC("import") }
	}
	
	"TokenizerPC" should "handle an internal token" in {
		assertResult(List(InternalToken)) { TokenizerPC("internal") }
	}
	
	"TokenizerPC" should "handle a final token" in {
		assertResult(List(FinalToken)) { TokenizerPC("final") }
	}
	
	"TokenizerPC" should "handle an open token" in {
		assertResult(List(OpenToken)) { TokenizerPC("open") }
	}
	
	"TokenizerPC" should "handle a private token" in {
		assertResult(List(PrivateToken)) { TokenizerPC("private") }
	}
	
	"TokenizerPC" should "handle a public token" in {
		assertResult(List(PublicToken)) { TokenizerPC("public") }
	}
	
	"TokenizerPC" should "handle a where token" in {
		assertResult(List(WhereToken)) { TokenizerPC("where") }
	}
	
	"TokenizerPC" should "handle a while token" in {
		assertResult(List(WhileToken)) { TokenizerPC("while") }
	}
	
	"TokenizerPC" should "handle a let token" in {
		assertResult(List(LetToken)) { TokenizerPC("let") }
	}
	
	"TokenizerPC" should "handle a var token" in {
		assertResult(List(VarToken)) { TokenizerPC("var") }
	}
	
	"TokenizerPC" should "handle a protocol token" in {
		assertResult(List(ProtocolToken)) { TokenizerPC("protocol") }
	}
	
	"TokenizerPC" should "handle a get token" in {
		assertResult(List(GetToken)) { TokenizerPC("get") }
	}
	
	"TokenizerPC" should "handle a set token" in {
		assertResult(List(SetToken)) { TokenizerPC("set") }
	}
	
	"TokenizerPC" should "handle a willSet token" in {
		assertResult(List(WillSetToken)) { TokenizerPC("willSet") }
	}
	
	"TokenizerPC" should "handle a didSet token" in {
		assertResult(List(DidSetToken)) { TokenizerPC("didSet") }
	}
	
	"TokenizerPC" should "handle a repeat token" in {
		assertResult(List(RepeatToken)) { TokenizerPC("repeat") }
	}
	
	"TokenizerPC" should "handle a switch token" in {
		assertResult(List(SwitchToken)) { TokenizerPC("switch") }
	}
	
	"TokenizerPC" should "handle a struct token" in {
		assertResult(List(StructToken)) { TokenizerPC("struct") }
	}
	
	"TokenizerPC" should "handle a return token" in {
		assertResult(List(ReturnToken)) { TokenizerPC("return") }
	}
	
	"TokenizerPC" should "handle a throw token" in {
		assertResult(List(ThrowToken)) { TokenizerPC("throw") }
	}
	
	"TokenizerPC" should "handle a throws token" in {
		assertResult(List(ThrowsToken)) { TokenizerPC("throws") }
	}
	
	"TokenizerPC" should "handle a rethrows token" in {
		assertResult(List(RethrowsToken)) { TokenizerPC("rethrows") }
	}
	
	"TokenizerPC" should "handle an indirect token" in {
		assertResult(List(IndirectToken)) { TokenizerPC("indirect") }
	}
	
	"TokenizerPC" should "handle an init token" in {
		assertResult(List(InitToken)) { TokenizerPC("init") }
	}
	
	"TokenizerPC" should "handle a deinit token" in {
		assertResult(List(DeinitToken)) { TokenizerPC("deinit") }
	}
	
	"TokenizerPC" should "handle an associatedType token" in {
		assertResult(List(AssociatedTypeToken)) { TokenizerPC("associatedtype") }
	}
	
	"TokenizerPC" should "handle an extension token" in {
		assertResult(List(ExtensionToken)) { TokenizerPC("extension") }
	}
	
	"TokenizerPC" should "handle a subscript token" in {
		assertResult(List(SubscriptToken)) { TokenizerPC("subscript") }
	}
	
	"TokenizerPC" should "handle a prefix token" in {
		assertResult(List(PrefixToken)) { TokenizerPC("prefix") }
	}
	
	"TokenizerPC" should "handle an infix token" in {
		assertResult(List(InfixToken)) { TokenizerPC("infix") }
	}
	
	"TokenizerPC" should "handle a left token" in {
		assertResult(List(LeftToken)) { TokenizerPC("left") }
	}
	
	"TokenizerPC" should "handle a right token" in {
		assertResult(List(RightToken)) { TokenizerPC("right") }
	}
	
	"TokenizerPC" should "handle a none token" in {
		assertResult(List(NoneToken)) { TokenizerPC("none") }
	}
	
	"TokenizerPC" should "handle a precedencegroup token" in {
		assertResult(List(PrecedenceGroupToken)) { TokenizerPC("precedencegroup") }
	}
	
	"TokenizerPC" should "handle a higherThan token" in {
		assertResult(List(HigherThanToken)) { TokenizerPC("higherThan") }
	}
	
	"TokenizerPC" should "handle a lowerThan token" in {
		assertResult(List(LowerThanToken)) { TokenizerPC("lowerThan") }
	}
	
	"TokenizerPC" should "handle an assignment token" in {
		assertResult(List(AssignmentToken)) { TokenizerPC("assignment") }
	}
	
	"TokenizerPC" should "handle an associativity token" in {
		assertResult(List(AssociativityToken)) { TokenizerPC("associativity") }
	}
	
	"TokenizerPC" should "handle a postfix token" in {
		assertResult(List(PostFixToken)) { TokenizerPC("postfix") }
	}
	
	"TokenizerPC" should "handle an operator token" in {
		assertResult(List(OperatorToken)) { TokenizerPC("operator") }
	}
	
	"TokenizerPC" should "handle a typealias token" in {
		assertResult(List(TypeAliasToken)) { TokenizerPC("typealias") }
	}
	
	"TokenizerPC" should "handle an os token" in {
		assertResult(List(OSToken)) { TokenizerPC("os") }
	}
	
	"TokenizerPC" should "handle an arch token" in {
		assertResult(List(ArchToken)) { TokenizerPC("arch") }
	}
	
	"TokenizerPC" should "handle a swift token" in {
		assertResult(List(SwiftToken)) { TokenizerPC("swift") }
	}
	
	"TokenizerPC" should "handle a compiler token" in {
		assertResult(List(CompilerToken)) { TokenizerPC("compiler") }
	}
	
	"TokenizerPC" should "handle a canImport token" in {
		assertResult(List(CanImportToken)) { TokenizerPC("canImport") }
	}
	
	"TokenizerPC" should "handle a targetEnvironment token" in {
		assertResult(List(TargetEnvironmentToken)) { TokenizerPC("targetEnvironment") }
	}
	
	"TokenizerPC" should "handle a convenience token" in {
		assertResult(List(ConvenienceToken)) { TokenizerPC("convenience") }
	}
	
	"TokenizerPC" should "handle a dynamic token" in {
		assertResult(List(DynamicToken)) { TokenizerPC("dynamic") }
	}
	
	"TokenizerPC" should "handle a lazy token" in {
		assertResult(List(LazyToken)) { TokenizerPC("lazy") }
	}
	
	"TokenizerPC" should "handle an optional token" in {
		assertResult(List(OptionalToken)) { TokenizerPC("optional") }
	}
	
	"TokenizerPC" should "handle an override token" in {
		assertResult(List(OverrideToken)) { TokenizerPC("override") }
	}
	
	"TokenizerPC" should "handle a required token" in {
		assertResult(List(RequiredToken)) { TokenizerPC("required") }
	}
	
	"TokenizerPC" should "handle a static token" in {
		assertResult(List(StaticToken)) { TokenizerPC("static") }
	}
	
	"TokenizerPC" should "handle a weak token" in {
		assertResult(List(WeakToken)) { TokenizerPC("weak") }
	}
	
	"TokenizerPC" should "handle an unowned token" in {
		assertResult(List(UnownedToken)) { TokenizerPC("unowned") }
	}
	
	"TokenizerPC" should "handle a safe token" in {
		assertResult(List(SafeToken)) { TokenizerPC("safe") }
	}
	
	"TokenizerPC" should "handle an unsafe token" in {
		assertResult(List(UnsafeToken)) { TokenizerPC("unsafe") }
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
