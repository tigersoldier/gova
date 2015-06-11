package token

type Token int

const (
	ILLEGAL Token = iota
	EOF
	COMMENT
	WHITE_SPACE
	NEWLINE
	IDENT

	keyword_begin
	ABSTRACT
	ASSERT
	BOOLEAN
	BREAK
	BYTE
	CASE
	CATCH
	CHAR
	CLASS
	CONST
	CONTINUE
	DEFAULT
	DO
	DOUBLE
	ELSE
	ENUM
	EXTENDS
	FINAL
	FINALLY
	FLOAT
	FOR
	IF
	GOTO
	IMPLEMENTS
	IMPORT
	INSTANCEOF
	INT
	INTERFACE
	LONG
	NATIVE
	NEW
	PACKAGE
	PRIVATE
	PROTECTED
	PUBLIC
	RETURN
	SHORT
	STATIC
	STRICTFP
	SUPER
	SWITCH
	SYNCHRONIZED
	THIS
	THROW
	THROWS
	TRANSIENT
	TRY
	VOID
	VOLATILE
	WHILE
	keyword_end

	literal_begin
	INTNUM   // 1234
	FLOATNUM // 123.4
	CHARLIT  // 'c'
	STRING   // "st"

	TRUE
	FALSE
	NULL
	literal_end

	separator_begin
	LPAREN // (
	RPAREN // )
	LBRACK // [
	RBRACK // ]
	LBRACE // {
	RBRACE // }

	SEMICOLON    // ;
	COMMA        // ,
	DOT          // .
	ELLIPSE      // ...
	AT           // @
	DOUBLE_COLON // ::
	separator_end

	operator_begin
	GT  // >
	LT  // <
	NOT // !
	EQ  // ==
	GEQ // >=
	LEQ // <=
	NEQ // !=

	BITCOMP  // ~
	QUESTION // ?
	COLON    // :

	LAMBDA_ARROW // ->

	LAND // &&
	LOR  // ||

	INC // ++
	DEC // --

	ADD // +
	SUB // -
	MUL // *
	QUO // /
	REM // %

	AND // &
	OR  // |
	XOR // ^

	SHL  // <<
	SHR  // >>
	USHR // >>>

	ASSIGN // =

	ADD_ASSIGN // +=
	SUB_ASSIGN // -=
	MUL_ASSIGN // *=
	QUO_ASSIGN // /=
	REM_ASSIGN // %=

	AND_ASSIGN // &=
	OR_ASSIGN  // |=
	XOR_ASSIGN // ^=

	SHL_ASSIGN  // <<=
	SHR_ASSIGN  // >>=
	USHR_ASSIGN // >>>=
	operator_end
)

var tokenstrs = [...]string{
	ILLEGAL:     "ILLEGAL",
	EOF:         "EOF",
	COMMENT:     "COMMENT",
	WHITE_SPACE: "WHITE_SPACE",
	NEWLINE:     "NEWLINE",
	IDENT:       "IDENT",

	// keyword_beg
	ABSTRACT:     "abstract",
	ASSERT:       "assert",
	BOOLEAN:      "boolean",
	BREAK:        "break",
	BYTE:         "byte",
	CASE:         "case",
	CATCH:        "catch",
	CHAR:         "char",
	CLASS:        "class",
	CONST:        "const",
	CONTINUE:     "continue",
	DEFAULT:      "default",
	DO:           "do",
	DOUBLE:       "double",
	ELSE:         "else",
	ENUM:         "enum",
	EXTENDS:      "extends",
	FINAL:        "final",
	FINALLY:      "finally",
	FLOAT:        "float",
	FOR:          "for",
	IF:           "if",
	GOTO:         "goto",
	IMPLEMENTS:   "implements",
	IMPORT:       "import",
	INSTANCEOF:   "instanceof",
	INT:          "int",
	INTERFACE:    "interface",
	LONG:         "long",
	NATIVE:       "native",
	NEW:          "new",
	PACKAGE:      "package",
	PRIVATE:      "private",
	PROTECTED:    "protected",
	PUBLIC:       "public",
	RETURN:       "return",
	SHORT:        "short",
	STATIC:       "static",
	STRICTFP:     "strictfp",
	SUPER:        "super",
	SWITCH:       "switch",
	SYNCHRONIZED: "synchronized",
	THIS:         "this",
	THROW:        "throw",
	THROWS:       "throws",
	TRANSIENT:    "transient",
	TRY:          "try",
	VOID:         "void",
	VOLATILE:     "volatile",
	WHILE:        "while",
	// keyword_end

	// literal_begin
	INTNUM:   "INTNUM",
	FLOATNUM: "FLOATNUM",
	CHARLIT:  "CHARLIT",
	STRING:   "STRING",

	TRUE:  "true",
	FALSE: "false",
	NULL:  "null",
	// literal_end

	// separator_begin
	LPAREN: "(", // (
	RPAREN: ")", // )
	LBRACK: "[", // [
	RBRACK: "]", // ]
	LBRACE: "{", // {
	RBRACE: "}", // }

	SEMICOLON:    ";",
	COMMA:        ",",
	DOT:          ".",
	ELLIPSE:      "...",
	AT:           "@",
	DOUBLE_COLON: "::",
	// separator_end

	// operator_begin
	GT:  ">",
	LT:  "<",
	NOT: "!",
	EQ:  "==",
	GEQ: ">=",
	LEQ: "<=",
	NEQ: "!=",

	BITCOMP:  "~",
	QUESTION: "?",
	COLON:    ":",

	LAMBDA_ARROW: "->",

	LAND: "&&",
	LOR:  "||",

	INC: "++",
	DEC: "--",

	ADD: "+",
	SUB: "-",
	MUL: "*",
	QUO: "/",
	REM: "%",

	AND: "&",
	OR:  "|",
	XOR: "^",

	SHL:  "<<",
	SHR:  ">>",
	USHR: ">>>",

	ASSIGN: "=",

	ADD_ASSIGN: "+=",
	SUB_ASSIGN: "-=",
	MUL_ASSIGN: "*=",
	QUO_ASSIGN: "/=",
	REM_ASSIGN: "%=",

	AND_ASSIGN: "&=",
	OR_ASSIGN:  "|=",
	XOR_ASSIGN: "^=",

	SHL_ASSIGN:  "<<=",
	SHR_ASSIGN:  ">>=",
	USHR_ASSIGN: ">>>=",
	// operator_end
}

var litToToken map[string]Token

func init() {
	litToToken = make(map[string]Token)
	for tok := keyword_begin + 1; tok < keyword_end; tok++ {
		litToToken[tokenstrs[tok]] = tok
	}
	// keyword-like literals
	for _, tok := range []Token{TRUE, FALSE, NULL} {
		litToToken[tokenstrs[tok]] = tok
	}
}

func (t Token) String() string {
	if int(t) >= len(tokenstrs) || int(t) < 0 {
		return "token(" + string(int(t)) + ")"
	}
	return tokenstrs[t]
}

func LookupKeywordLike(lit string) Token {
	if tok, found := litToToken[lit]; found {
		return tok
	}
	return IDENT
}
