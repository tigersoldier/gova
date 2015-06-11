package scanner

import (
	"bytes"
	"github.com/tigersoldier/gova/token"
	"strings"
	"testing"
)

type scanResult struct {
	tok token.Token
	lit string
}

var charEscapes = []struct {
	lit string
	ch  rune
}{
	{`\b`, '\b'},
	{`\t`, '\t'},
	{`\n`, '\n'},
	{`\f`, '\f'},
	{`\r`, '\r'},
	{`\"`, '"'},
	{`\'`, '\''},
	{`\\`, '\\'},
	// Octal values
	{`\0`, 0},
	{`\00`, 0},
	{`\000`, 0},
	{`\7`, 07},
	{`\07`, 07},
	{`\007`, 07},
	{`\10`, 010},
	{`\77`, 077},
	{`\100`, 0100},
	{`\377`, 0377},
}

func TestScannerNext(t *testing.T) {
	var scanTests = []struct {
		src     string
		results []rune
	}{
		{"\ufeff", []rune{'\ufeff'}},
		{"test", []rune{'t', 'e', 's', 't'}},
		{"世界", []rune{'世', '界'}},
		// Unicode escape
		{"\\u65E5\\u672C\\u8A9E", []rune{'日', '本', '語'}},
		{"\\\\u65E5ha\\u672C", []rune{'\\', '\\', 'u', '6', '5', 'E', '5', 'h', 'a', '本'}},
		{"\\\\\\u65E5", []rune{'\\', '\\', '日'}},
		{"\\uuuuuu65E5", []rune{'日'}},
	}

	for _, test := range scanTests {
		scanner := &Scanner{src: []byte(test.src), offset: 0, nextOffset: 0}
		for index, result := range test.results {
			scanner.next()
			if scanner.ch != result {
				t.Errorf("Expecting the %dth next() result of src %q to be %q, got %q",
					index, test.src, result, scanner.ch)
			}
		}

		scanner.next()
		if scanner.ch != -1 {
			t.Errorf("Expecting EOF after %d scans of %q, got %q",
				len(test.results), test.src, scanner.ch)
		}
		if scanner.offset != len(test.src) {
			t.Errorf("Expecting offset to be %d after %d scans of %q, got %d",
				len(test.src), len(test.results), test.src, scanner.offset)
		}
		if scanner.nextOffset != len(test.src) {
			t.Errorf("Expecting nextOffset to be %d after %d scans of %q, got %d",
				len(test.src), len(test.results), test.src, scanner.nextOffset)
		}
	}
}

func TestScannerNext_invalid(t *testing.T) {
	tests := []struct {
		src      string
		numScans int
	}{
		{"A" + string(bom), 2}, // BOM can only be the first charactor
		{"\xFC\x0F", 1},        // Invalid UTF-8
	}

	for _, test := range tests {
		var scanner Scanner
		scanner.Init([]byte(test.src))
		for i := 0; i < test.numScans; i++ {
			if scanner.ch == -1 {
				t.Errorf("The %dth next() invoke on %q shouldn't have error", i, test.src)
			}
			scanner.next()
		}
		scanner.next()
		if scanner.ch != -1 {
			t.Errorf("The %th next() invoke on %q should be error, got %q", test.numScans, test.src, scanner.ch)
		}
	}
}

func TestScanComments(t *testing.T) {
	src := `// End of line comment
            // End of line comment 2 /* */ // / /=
            /* Traditional comment */
            /* Traditional // comment
               with new line * / /= /**/
`
	results := []scanResult{
		{token.COMMENT, "// End of line comment"},
		{token.NEWLINE, "\n"},
		{token.COMMENT, "// End of line comment 2 /* */ // / /="},
		{token.NEWLINE, "\n"},
		{token.COMMENT, "/* Traditional comment */"},
		{token.NEWLINE, "\n"},
		{token.COMMENT, "/* Traditional // comment\n               with new line * / /= /**/"},
		{token.NEWLINE, "\n"},
	}
	testScanResults(src, results, false /* isInTypeContext */, t)
}

func TestScanSeparatorsAndOperators(t *testing.T) {
	src := "( ) { } [ ] " +
		"; , . ... @ :: " +
		"= > < ! ~ ? : " +
		"-> == >= <= != && || " +
		"++ -- + - * / & | " +
		"^ % << >> >>> += -= " +
		"*= /= &= |= ^= " +
		"%= <<= >>= >>>="
	tokens := []token.Token{
		token.LPAREN, token.RPAREN, token.LBRACE, token.RBRACE, token.LBRACK, token.RBRACK,
		token.SEMICOLON, token.COMMA, token.DOT, token.ELLIPSE, token.AT, token.DOUBLE_COLON,
		token.ASSIGN, token.GT, token.LT, token.NOT, token.BITCOMP, token.QUESTION, token.COLON,
		token.LAMBDA_ARROW, token.EQ, token.GEQ, token.LEQ, token.NEQ, token.LAND, token.LOR,
		token.INC, token.DEC, token.ADD, token.SUB, token.MUL, token.QUO, token.AND, token.OR,
		token.XOR, token.REM, token.SHL, token.SHR, token.USHR, token.ADD_ASSIGN, token.SUB_ASSIGN,
		token.MUL_ASSIGN, token.QUO_ASSIGN, token.AND_ASSIGN, token.OR_ASSIGN, token.XOR_ASSIGN,
		token.REM_ASSIGN, token.SHL_ASSIGN, token.SHR_ASSIGN, token.USHR_ASSIGN,
	}
	testScanTokens(src, tokens, false /* isInTypeContext */, t)
}

func TestScanKeywordLike(t *testing.T) {
	src := "abstract continue for new switch " +
		"assert default if package synchronized " +
		"boolean do goto private this " +
		"break double implements protected throw " +
		"byte else import public throws " +
		"case enum instanceof return transient " +
		"catch extends int short try " +
		"char final interface static void " +
		"class finally long strictfp volatile " +
		"const float native super while " +
		"true false null"
	tokens := []token.Token{
		token.ABSTRACT, token.CONTINUE, token.FOR, token.NEW, token.SWITCH,
		token.ASSERT, token.DEFAULT, token.IF, token.PACKAGE, token.SYNCHRONIZED,
		token.BOOLEAN, token.DO, token.GOTO, token.PRIVATE, token.THIS,
		token.BREAK, token.DOUBLE, token.IMPLEMENTS, token.PROTECTED, token.THROW,
		token.BYTE, token.ELSE, token.IMPORT, token.PUBLIC, token.THROWS,
		token.CASE, token.ENUM, token.INSTANCEOF, token.RETURN, token.TRANSIENT,
		token.CATCH, token.EXTENDS, token.INT, token.SHORT, token.TRY,
		token.CHAR, token.FINAL, token.INTERFACE, token.STATIC, token.VOID,
		token.CLASS, token.FINALLY, token.LONG, token.STRICTFP, token.VOLATILE,
		token.CONST, token.FLOAT, token.NATIVE, token.SUPER, token.WHILE,
		token.TRUE, token.FALSE, token.NULL,
	}
	testScanTokens(src, tokens, false /* isInTypeContext */, t)
}

func TestScanGreaterSigns(t *testing.T) {
	src := "> >> >>> >= >>= >>>="
	typeContextTokens := []token.Token{
		token.GT,
		token.GT, token.GT,
		token.GT, token.GT, token.GT,
		token.GT, token.ASSIGN,
		token.GT, token.GT, token.ASSIGN,
		token.GT, token.GT, token.GT, token.ASSIGN,
	}
	notTypeContextTokens := []token.Token{
		token.GT, token.SHR, token.USHR,
		token.GEQ, token.SHR_ASSIGN, token.USHR_ASSIGN,
	}
	testScanTokens(src, typeContextTokens, true, t)
	testScanTokens(src, notTypeContextTokens, false, t)
}

func TestScanIdentifiers(t *testing.T) {
	src := "_ $ ident _123 _i1 _abstract $for $_1a_$"
	lits := strings.Split(src, " ")
	results := make([]scanResult, len(lits), len(lits))
	for i, lit := range lits {
		results[i] = scanResult{token.IDENT, lit}
	}
	testScanResults(src, results, false /* isInTypeContext */, t)
}

func TestScanInteger(t *testing.T) {
	src := "0 00 0x0 0b0 " + // zeros
		"1234567890 " + // decimal
		"0b01010010 0B010101 " + // binary
		"012345670 " + // octal
		"0x0123456789abcdef 0XFEDCBA9876543210 " + // hex
		"1_2_3l 0x1_a_bL 0b1_0l 0_1_2 " + // underscores and suffix
		// errors
		"012345_ 012345_l " + // trailing underscore
		"0x_123 0b_101 " + // leading underscore
		"0xl 0x 0bl 0b " + // no hex or binary digit
		// invalid characters are considered starting a new token
		"0123456789 123456789abc " +
		"0xabcdefg 0b01234 " +
		"01234567lmn 1234lmn " +
		"0xabclmn 0b010lmn "
	results := []scanResult{
		{token.INTNUM, "0"}, {token.INTNUM, "00"}, {token.INTNUM, "0x0"}, {token.INTNUM, "0b0"},
		{token.INTNUM, "1234567890"},
		{token.INTNUM, "0b01010010"}, {token.INTNUM, "0B010101"},
		{token.INTNUM, "012345670"},
		{token.INTNUM, "0x0123456789abcdef"}, {token.INTNUM, "0XFEDCBA9876543210"},
		{token.INTNUM, "1_2_3l"}, {token.INTNUM, "0x1_a_bL"}, {token.INTNUM, "0b1_0l"}, {token.INTNUM, "0_1_2"},
		{token.ILLEGAL, "012345_"}, {token.ILLEGAL, "012345_l"},
		{token.ILLEGAL, "0x_123"}, {token.ILLEGAL, "0b_101"},
		{token.ILLEGAL, "0xl"}, {token.ILLEGAL, "0x"}, {token.ILLEGAL, "0bl"}, {token.ILLEGAL, "0b"},
		{token.ILLEGAL, "0123456789"}, {token.INTNUM, "123456789"}, {token.IDENT, "abc"},
		{token.INTNUM, "0xabcdef"}, {token.IDENT, "g"}, {token.INTNUM, "0b01"}, {token.INTNUM, "234"},
		{token.INTNUM, "01234567l"}, {token.IDENT, "mn"}, {token.INTNUM, "1234l"}, {token.IDENT, "mn"},
		{token.INTNUM, "0xabcl"}, {token.IDENT, "mn"}, {token.INTNUM, "0b010l"}, {token.IDENT, "mn"},
	}
	testScanResults(src, results, false /* isInTypeContext */, t)
}

func TestScanFloatingPoint(t *testing.T) {
	src := "0f 123_456_789f" +
		"09F " + // 0-prefixed number isn't octal for floating-point numbers
		"0.12_3 0.123f 0.123F " +
		".1_23 .123d .123D " +
		"12_34e1 12.34e+1 .1234e-1 " +
		"123.f 123.e1 " +
		// hex form
		"0xabc_defp1234_567890 " +
		"0Xabcdefp1234567890f " +
		"0xabcdefp1234567890d " +
		"0xabcdef.p1234567890 " +
		"0x.abcdefp123 " +
		"0X.abcdefp123F " +
		"0x.abcdefp123D " +
		"0xabcdef.abcdefp123 " +
		"0Xabcdef.abcdefp+123 " +
		"0xabcdef.abcdefp-123 "
	results := []scanResult{
		{token.FLOATNUM, "0f"},
		{token.FLOATNUM, "123_456_789f"},
		{token.FLOATNUM, "09F"},
		{token.FLOATNUM, "0.12_3"},
		{token.FLOATNUM, "0.123f"},
		{token.FLOATNUM, "0.123F"},
		{token.FLOATNUM, ".1_23"},
		{token.FLOATNUM, ".123d"},
		{token.FLOATNUM, ".123D"},
		{token.FLOATNUM, "12_34e1"},
		{token.FLOATNUM, "12.34e+1"},
		{token.FLOATNUM, ".1234e-1"},
		{token.FLOATNUM, "123.f"},
		{token.FLOATNUM, "123.e1"},
		{token.FLOATNUM, "0xabc_defp1234_567890"},
		{token.FLOATNUM, "0Xabcdefp1234567890f"},
		{token.FLOATNUM, "0xabcdefp1234567890d"},
		{token.FLOATNUM, "0xabcdef.p1234567890"},
		{token.FLOATNUM, "0x.abcdefp123"},
		{token.FLOATNUM, "0X.abcdefp123F"},
		{token.FLOATNUM, "0x.abcdefp123D"},
		{token.FLOATNUM, "0xabcdef.abcdefp123"},
		{token.FLOATNUM, "0Xabcdef.abcdefp+123"},
		{token.FLOATNUM, "0xabcdef.abcdefp-123"},
	}
	testScanResults(src, results, false /* isInTypeContext */, t)
}

func TestScanIllegalFloatingPoints(t *testing.T) {
	illegalFloatingPoints := []string{
		// Dec form
		"123e", "123e+", "123e-", "123ef", // Illegal exponent
		"1_.", "1._234", "1.234_", ".234_", // Leading/trailing underscore
		// Hex form
		"0x.p1f", "0xp1f", // Illegal fraction
		"0x0.0", "0x.0", "0x0.", "0x0p", "0x0pf", // Illegal exponent
		"0x_0.0p1", "0x0_.0p1", "0x._0p1", "0x.0_p1", // Leading/trailing underscore
	}
	testIllegalSrcs(illegalFloatingPoints, t)
}

func TestScanUnescapedChars(t *testing.T) {
	var buf bytes.Buffer
	unescapedChars := []rune{'a', 'b', 'c', 'A', 'B', 'C', '世', ' ', '\uFFFF'}
	unescapedResults := make([]scanResult, len(unescapedChars), len(unescapedChars))
	for index, ch := range unescapedChars {
		buf.WriteRune('\'')
		buf.WriteRune(ch)
		buf.WriteRune('\'')
		unescapedResults[index] = scanResult{token.CHARLIT, string(ch)}
	}
	testScanResults(buf.String(), unescapedResults, false /* isInTypeContext */, t)
}

func TestScanEscapedChars(t *testing.T) {
	var buf bytes.Buffer
	escapedResults := make([]scanResult, len(charEscapes), len(charEscapes))
	for index, escapeCase := range charEscapes {
		buf.WriteRune('\'')
		buf.WriteString(escapeCase.lit)
		buf.WriteRune('\'')
		escapedResults[index] = scanResult{token.CHARLIT, string(escapeCase.ch)}
	}
	testScanResults(buf.String(), escapedResults, false /* isInTypeContext */, t)
}

func TestScanIllegalChars(t *testing.T) {
	illegalChars := []string{"'\r'", "'\n'", "'", "'''", "'abc'", `'\u10000'`, `'\c'`, `'\400'`}
	testIllegalSrcs(illegalChars, t)
}

func TestScanUnescapedString(t *testing.T) {
	var srcbuf bytes.Buffer

	validStrings := []string{"The quick fox ", "jumps over", " the lazy dog"}
	validResults := make([]scanResult, len(validStrings), len(validStrings))
	for index, str := range validStrings {
		srcbuf.WriteRune('"')
		srcbuf.WriteString(str)
		srcbuf.WriteRune('"')
		validResults[index] = scanResult{token.STRING, str}
	}
	testScanResults(srcbuf.String(), validResults, false /* isInTypeContext */, t)
}

func TestScanEscapedString(t *testing.T) {
	var srcbuf bytes.Buffer
	var litbuf bytes.Buffer

	srcbuf.WriteRune('"')
	for _, escapedCase := range charEscapes {
		srcbuf.WriteString(escapedCase.lit)
		litbuf.WriteRune(escapedCase.ch)
	}
	srcbuf.WriteRune('"')
	escapedResults := []scanResult{{token.STRING, litbuf.String()}}
	t.Logf("%s -> %q", srcbuf.String(), litbuf.String())
	testScanResults(srcbuf.String(), escapedResults, false /* isInTypeContext */, t)
}

func TestScanIllegalString(t *testing.T) {
	illegalCases := []struct {
		src         string
		returnedLit string
	}{
		{`"without close quote`, "without close quote"},
		{"\"has line feed \n\"", "has line feed "},
		{"\"has carrage return \r\"", "has carrage return "},
	}
	for _, illegalCase := range illegalCases {
		scanner := newScanner(illegalCase.src)
		assertScanResult(
			scanner,
			0, /* index */
			scanResult{token.ILLEGAL, illegalCase.returnedLit},
			false, /* isInTypeContext */
			t)
	}
}

func testScanTokens(src string, tokens []token.Token, isInTypeContext bool, t *testing.T) {
	results := make([]scanResult, len(tokens), len(tokens))
	for index, token := range tokens {
		results[index].tok = token
		results[index].lit = token.String()
	}
	testScanResults(src, results, isInTypeContext, t)
}

func testIllegalSrcs(srcs []string, t *testing.T) {
	for _, src := range srcs {
		scanner := newScanner(src)
		if tok, lit := scanner.Scan(false /* isInTypeContext */); tok != token.ILLEGAL {
			t.Errorf("expecting ILLEGAL for %q, got %s of %q", src, tok, lit)
		}
	}
}

func testScanResults(src string, results []scanResult, isInTypeContext bool, t *testing.T) {
	scanner := newScanner(src)
	for resultIndex, result := range results {
		assertScanResult(scanner, resultIndex, result, isInTypeContext, t)
	}
	tok, lit := scanner.Scan(isInTypeContext)
	if tok != token.EOF {
		t.Errorf("expecting EOF on the %dth scan, got %s %q", len(results), tok, lit)
	}
}

func assertScanResult(scanner *Scanner, index int, result scanResult, isInTypeContext bool, t *testing.T) {
	tok, lit := scanner.Scan(isInTypeContext)
	if tok != result.tok {
		t.Errorf("expecting token %s on the %dth scan, got %s", result.tok, index, tok)
	}
	if lit != result.lit {
		t.Errorf("expecting lit %q on the %dth scan, got %q", result.lit, index, lit)
	}
}

func newScanner(src string) (scanner *Scanner) {
	scanner = &Scanner{}
	scanner.Init([]byte(src))
	return
}
