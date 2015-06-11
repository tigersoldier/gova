package scanner

import (
	"bytes"
	"fmt"
	"github.com/tigersoldier/gova/token"
	"unicode"
	"unicode/utf8"
)

const bom = 0xFEFF // byte order mark, only permitted as very first character
const eof rune = -1
const illegal rune = 0

type Scanner struct {
	src            []byte
	offset         int
	nextOffset     int
	ch             rune
	numBackSlashes int
}

func (s *Scanner) Init(src []byte) {
	s.src = src
	s.offset = 0
	s.nextOffset = 0
	s.numBackSlashes = 0
	s.next()
	if s.ch == bom {
		s.next()
	}
}

func (s *Scanner) next() {
	s.offset = s.nextOffset

	if s.nextOffset == len(s.src) {
		s.ch = eof
		return
	}

	runeValue := rune(s.src[s.nextOffset])
	width := 1

	switch {
	case runeValue == '\\' && s.numBackSlashes%2 == 0:
		runeValue, width = tryDecodeUnicodeEscape(s.src[s.nextOffset:])
	case runeValue >= '\x80':
		runeValue, width = utf8.DecodeRune(s.src[s.nextOffset:])
		if runeValue == 1 {
			// Error: invalid UTF-8
		}
		if runeValue == bom && s.offset != 0 {
			// Error: BOM can only be the first character
		}
	default:
		// TODO: handle new line
	}

	if runeValue == '\\' {
		s.numBackSlashes++
	} else {
		s.numBackSlashes = 0
	}

	s.ch = runeValue
	s.nextOffset += width
}

func tryDecodeUnicodeEscape(src []byte) (runeValue rune, width int) {
	if len(src) == 0 {
		// Shouldn't happen
		panic("trying to decode an empty src")
	}
	if src[0] != '\\' {
		panic(fmt.Sprintf("src %q not starts with backslash", src))
	}

	runeValue = '\\'
	width = 1

	// Unicode escape must have at least 6 characters: \uXXXX
	if len(src) < 6 {
		return
	}

	// Skip all preceeding 'u's
	if src[1] != 'u' {
		return
	}

	hexOffset := 2
	for ; src[hexOffset] == 'u' && hexOffset < len(src); hexOffset++ {
	}

	// Decode hexadecimal value
	if len(src) < hexOffset+4 {
		return
	}
	hexValue := 0
	for hexIndex := 0; hexIndex < 4; hexIndex++ {
		hexValue <<= 4
		switch ch := src[hexOffset+hexIndex]; {
		case '0' <= ch && ch <= '9':
			hexValue += int(ch - '0')
		case 'a' <= ch && ch <= 'f':
			hexValue += int(ch-'a') + 10
		case 'A' <= ch && ch <= 'f':
			hexValue += int(ch-'A') + 10
		default:
			return
		}
	}

	runeValue = rune(hexValue)
	width = hexOffset + 4
	return
}

func (s *Scanner) scanTraditionalComment() (token.Token, string) {
	startPos := s.offset - 1 // The preceeding "/" has been scanned
	// skip the next "*"
	s.next()

	for ; s.ch != eof; s.next() {
		if s.ch == '/' && s.src[s.offset-1] == '*' {
			s.next()
			return token.COMMENT, string(s.src[startPos:s.offset])
		}
	}
	return token.ILLEGAL, ""
}

func (s *Scanner) scanEndOfLineComment() (token.Token, string) {
	startPos := s.offset - 1 // The preceeding "/" has been scanned
	// skip the next "/"
	for ; s.ch != eof && s.ch != '\n' && s.ch != '\r'; s.next() {
	}
	return token.COMMENT, string(s.src[startPos:s.offset])
}

// scanMaybeSuffix checks if next character is the specified
// suffix. If yes, return suffixToken move next, otherwise return
// rawToken.
func (s *Scanner) scanMaybeSuffix(suffix rune, rawToken token.Token, suffixToken token.Token) (token.Token, string) {
	var tok token.Token
	if s.ch == suffix {
		s.next()
		tok = suffixToken
	} else {
		tok = rawToken
	}
	return tok, tok.String()
}

func (s *Scanner) scanLetterStart() (token.Token, string) {
	startPos := s.offset
	s.next()
	hasDigit := false
	for {
		if isJavaLetter(s.ch) {
			s.next()
		} else if unicode.IsDigit(s.ch) {
			s.next()
			hasDigit = true
		} else {
			break
		}
	}
	lit := string(s.src[startPos:s.offset])
	if hasDigit {
		// Keywords and keyword-like literals don't have digits
		return token.IDENT, lit
	} else {
		return token.LookupKeywordLike(lit), lit
	}
}

func (s *Scanner) scanNumber() (tok token.Token, lit string) {
	startOffset := s.offset
	startNextOffset := s.nextOffset
	startsWithZero := false
	base := 10
	
	if s.ch == '0' {
		startsWithZero = true
		s.next()
		if s.ch == 'x' || s.ch == 'X' {
			base = 16
			s.next()
			if s.ch == '.' {
				s.next()
				return s.scanFraction(base), string(s.src[startOffset:s.offset])
			}
		} else if s.ch == 'b' || s.ch == 'B' {
			base = 2
			s.next()
		} else {
			// It can be an octal integer, or a
			// floating-point number. We use decimal
			// predicate, and handle the octal integer
			// case later

			// Move back so that the next underscore won't
			// be considered as leading underscore
			s.ch = '0'
			s.offset = startOffset
			s.nextOffset = startNextOffset
		}
	}

	hasValidDigits := s.scanDigits(base)
	tok = token.INTNUM

	if s.ch == 'l' || s.ch == 'L' {
		// long integer suffix
		s.next()
	} else if base == 10 || base == 16 {
		// Check floating point formats
		if s.ch == '.' {
			s.next()
			if isExponentPrefix(s.ch, base) {
				tok = s.scanExponent(base)
			} else if base == 10 && !isDecDigit(s.ch) && s.ch != '_' {
				// Floating point form of 123.[f|F|d|D]
				if isFloatSuffix(s.ch) {
					s.next()
				}
				tok = token.FLOATNUM
			} else {
				tok = s.scanFraction(base)
			}
		} else if isExponentPrefix(s.ch, base) {
			tok = s.scanExponent(base)
		} else if isFloatSuffix(s.ch) {
			tok = token.FLOATNUM
			s.next()
		}
	}

	lit = string(s.src[startOffset:s.offset])
	if !hasValidDigits {
		tok = token.ILLEGAL
	}

	if tok == token.INTNUM && base == 10 && startsWithZero {
		// Validate octal integer
		for offset := startOffset + 1; offset < s.offset; offset++ {
			if s.src[offset] == '8' || s.src[offset] == '9' {
				tok = token.ILLEGAL
				break;
			}
		}
	}

	return
}

// scanDigit scans the next digits or underscores.
// Only 2, 8, 10, 16-based digits are supported
func (s *Scanner) scanDigits(digitBase int) (hasDigit bool) {
	var digitPredicate func (ch rune) bool
	switch digitBase {
	case 10:
		digitPredicate = isDecDigit
	case 16:
		digitPredicate = isHexDigit
	case 2:
		digitPredicate = isBinDigit
	case 8:
		digitPredicate = isOctDigit
	}
	hasLeadingUnderscore := false
	if s.ch == '_' {
		hasLeadingUnderscore = true
	}
	startPos := s.offset
	hasTrailingUnderscore := false
	for {
		if s.ch == '_' {
			hasTrailingUnderscore = true
		} else if digitPredicate(s.ch) {
			hasTrailingUnderscore = false
		} else {
			break;
		}
		s.next()
	}
	return !hasLeadingUnderscore && !hasTrailingUnderscore && s.offset > startPos
}

func (s *Scanner) scanFraction(base int) token.Token {
	if !s.scanDigits(base) {
		return token.ILLEGAL
	}
	return s.scanExponent(base)
}

func (s *Scanner) scanExponent(base int) (tok token.Token) {
	if !isExponentPrefix(s.ch, base) {
		if base == 10 {
			tok = token.FLOATNUM
		} else {
			// Exponent part is required for hexadecimal floating points
			tok = token.ILLEGAL
		}
	} else {
		s.next()
		if s.ch == '+' || s.ch == '-' {
			s.next()
		}
		if s.scanDigits(10 /* base */) {
			tok = token.FLOATNUM
		} else {
			tok = token.ILLEGAL
		}
	}
	if isFloatSuffix(s.ch) {
		s.next()
	}
	return
}

func (s *Scanner) scanEscape() (ch rune, ok bool) {
	// The preceeding '\' is already scanned
	switch s.ch {
	case 'b':
		s.next();
		return '\u0008', true
	case 't':
		s.next();
		return '\u0009', true
	case 'n':
		s.next();
		return '\u000a', true
	case 'f':
		s.next();
		return '\u000c', true
	case 'r':
		s.next();
		return '\u000d', true
	case '"':
		s.next();
		return '\u0022', true
	case '\'':
		s.next();
		return '\u0027', true
	case '\\':
		s.next();
		return '\u005c', true
	}
	if !isOctDigit(s.ch) {
		return -1, false
	}
	// Octal digits
	firstDigit := s.ch
	ch = s.ch - '0'
	ok = true
	s.next()
	if isOctDigit(s.ch) {
		ch = ch * 8 + (s.ch - '0')
		s.next()
		if isOctDigit(s.ch) && '0' <= firstDigit && firstDigit <= '3' {
			ch = ch * 8 + (s.ch - '0')
			s.next()
		}
	}
	return
}

func (s *Scanner) scanCharacter() (tok token.Token, lit string) {
	ch := s.ch
	s.next()
	switch ch {
	case eof:
	case '\n', '\r', '\'':
		return token.ILLEGAL, ""
	case '\\':
		var ok bool
		if ch, ok = s.scanEscape(); !ok {
			return token.ILLEGAL, ""
		}
	}
	if ch > 0xFFFF {
		return token.ILLEGAL, ""
	}
	if s.ch != '\'' {
		return token.ILLEGAL, ""
	}
	s.next()
	return token.CHARLIT, string(ch)
}

func (s *Scanner) scanString() (tok token.Token, lit string) {
	// Preceeding " has been scanned
	var buffer bytes.Buffer
	tok = token.STRING
loop:
	for {
		ch := s.ch
		s.next()
		switch ch {
		case eof, '\r', '\n':
			tok = token.ILLEGAL
			break loop
		case '"':
			break loop
		case '\\':
			var ok bool
			if ch, ok = s.scanEscape(); !ok {
				tok = token.ILLEGAL
				break loop
			}
		}
		buffer.WriteRune(ch)
	}
	lit = buffer.String()
	return
}

// Scan returns the next token and it's corresponding string value in the source.
//
// It has special treatment for ">". If isInTypeContext is false, ">>"
// will be tokenized as a SHR token, and ">>>" will be tokenized as a
// USHR token. Otherwise, ">>" will be tokenized as two GT tokens, and
// ">>>" will be tokenized as three GT tokens, so that the ">" characters
// in generic types such as A<B<C>> and A<B<C<D>>> will be tokenized
// correctly.
func (s *Scanner) Scan(isInTypeContext bool) (tok token.Token, lit string) {
	s.skipWhitespaces()
	switch ch := s.ch; {
	case ch == eof:
		return token.EOF, ""
	case ch == '\r' || ch == '\n':
		return s.scanNewLine()
	case isJavaLetter(ch):
		return s.scanLetterStart()
	case isDecDigit(ch):
		return s.scanNumber()
	default:
		s.next()
		lit := string(ch)
		switch ch {
		case '\'':
			return s.scanCharacter()
		case '"':
			return s.scanString()
		case '/':
			switch s.ch {
			case '/':
				return s.scanEndOfLineComment()
			case '*':
				return s.scanTraditionalComment()
			default:
				return s.scanMaybeSuffix('=', token.QUO, token.QUO_ASSIGN)
			}
		case '(':
			return token.LPAREN, lit
		case ')':
			return token.RPAREN, lit
		case '[':
			return token.LBRACK, lit
		case ']':
			return token.RBRACK, lit
		case '{':
			return token.LBRACE, lit
		case '}':
			return token.RBRACE, lit
		case ';':
			return token.SEMICOLON, lit
		case ',':
			return token.COMMA, lit
		case '.':
			if isDecDigit(s.ch) {
				startOffset := s.offset - 1
				tok = s.scanFraction(10)
				lit = string(s.src[startOffset:s.offset])
				return tok, lit
			}
			if s.ch == '.' && s.nextOffset < len(s.src) && s.src[s.nextOffset] == '.' {
				s.next()
				s.next()
				return token.ELLIPSE, "..."
			}
			return token.DOT, "."
		case '@':
			return token.AT, lit
		case ':':
			return s.scanMaybeSuffix(':', token.COLON, token.DOUBLE_COLON)
		case '>':
			// If '>' appears in type context, we always return token GT so that ">>" in
			// generic type A<B<C>> will be tokenized as two ">"s, not, a ">>" (SHR). The
			// similar rule applies to ">>>"
			if isInTypeContext {
				return token.GT, lit
			}
			if s.ch == '>' {
				s.next()
				if s.ch == '>' {
					s.next()
					return s.scanMaybeSuffix('=', token.USHR, token.USHR_ASSIGN)
				}
				return s.scanMaybeSuffix('=', token.SHR, token.SHR_ASSIGN)
			}
			return s.scanMaybeSuffix('=', token.GT, token.GEQ)
		case '<':
			if s.ch == '<' {
				s.next()
				return s.scanMaybeSuffix('=', token.SHL, token.SHL_ASSIGN)
			}
			return s.scanMaybeSuffix('=', token.LT, token.LEQ)
		case '!':
			return s.scanMaybeSuffix('=', token.NOT, token.NEQ)
		case '=':
			return s.scanMaybeSuffix('=', token.ASSIGN, token.EQ)
		case '~':
			return token.BITCOMP, lit
		case '?':
			return token.QUESTION, lit
		case '+':
			if s.ch == '+' {
				s.next()
				return token.INC, "++"
			}
			return s.scanMaybeSuffix('=', token.ADD, token.ADD_ASSIGN)
		case '-':
			if s.ch == '>' {
				s.next()
				return token.LAMBDA_ARROW, "->"
			}
			if s.ch == '-' {
				s.next()
				return token.DEC, "--"
			}
			return s.scanMaybeSuffix('=', token.SUB, token.SUB_ASSIGN)
		case '*':
			return s.scanMaybeSuffix('=', token.MUL, token.MUL_ASSIGN)
		case '%':
			return s.scanMaybeSuffix('=', token.REM, token.REM_ASSIGN)
		case '&':
			if s.ch == '&' {
				s.next()
				return token.LAND, "&&"
			}
			return s.scanMaybeSuffix('=', token.AND, token.AND_ASSIGN)
		case '|':
			if s.ch == '|' {
				s.next()
				return token.LOR, "||"
			}
			return s.scanMaybeSuffix('=', token.OR, token.OR_ASSIGN)
		case '^':
			return s.scanMaybeSuffix('=', token.XOR, token.XOR_ASSIGN)
		default:
			return token.ILLEGAL, string(ch)
		}
	}
}

func (s *Scanner) skipWhitespaces() {
	for s.ch == ' ' || s.ch == '\t' || s.ch == '\f' {
		s.next()
	}
}

func (s *Scanner) scanNewLine() (tok token.Token, lit string) {
	ch := s.ch
	lit = string(ch)
	s.next()
	if ch == '\r' && s.ch == '\n' {
		lit += "\n"
	}
	return token.NEWLINE, lit
}

func isJavaLetter(ch rune) bool {
	return ('A' <= ch && ch <= 'Z') ||
		('a' <= ch && ch <= 'z') ||
		ch == '_' || ch == '$'
}

func isDecDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func isOctDigit(ch rune) bool {
	return '0' <= ch && ch <= '7'
}

func isBinDigit(ch rune) bool {
	return ch == '0' || ch == '1'
}

func isHexDigit(ch rune) bool {
	return isDecDigit(ch) ||
		('a' <= ch && ch <= 'f') ||
		('A' <= ch && ch <= 'F')
}

func isFloatSuffix(ch rune) bool {
	return ch == 'f' || ch == 'F' || ch == 'd' || ch == 'D'
}

func isExponentPrefix(ch rune, base int) bool {
	if base == 10 {
		return ch == 'e' || ch == 'E'
	} else if base == 16 {
		return ch == 'p' || ch == 'P'
	} else {
		return false
	}
}
