package stinyc;
import java.io.Reader;
import java.io.IOException;

class CLex {

	String yytext = "";
	static final int EOF = -1;
	private int buf = -2;
	private Reader in = null;
	private Parser parser;
	public CLex(Reader r, Parser p) {
		this.in = r;
		this.parser = p;
	}

	// for debug
	private int getChar() throws IOException {
		if (buf == -2) {
			int c = in.read();
			return c;
		} else {
			int buf1 = buf;
			buf = -2;
			return buf1;
		}
	}

	private void ungetChar(int c) {
		buf = c;
	}

	private boolean isspace(int c) {
		if (c == ' ' || c == '\t' || c == '\r' || c == '\n') return true;
		else return false;
	}

	private boolean isdigit(int c) {
		if (c >= '0' && c <= '9') return true;
		else return false;
	}

	private boolean isalpha(int c) {
		if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') return true;
		else return false;
	}

	int yylex() throws IOException {
		int c = ' ';
		while (isspace(c)) {
			c = getChar();
		}

		switch (c) {
	    case '+':
		case '-':
		case '*':
		case '>':
		case '<':
		case '(':
		case ')':
		case '{':
		case '}':
		case ']':
		case '[':
		case ';':
		case ',':
		case '=':
		case EOF: return c;
		case '"': yytext = "";
			boolean flg = true;
			while (flg) {
				c = getChar();
				if (c == '"') {
					flg = false;
				} else {
					yytext += (char)c;
				}
			}
			parser.yylval = new ParserVal(yytext);
			return Parser.STRING;
		default:
			if (isdigit(c)) {
				int n = 0;
				do {
				    n = n * 10 + c - '0';
				    c = getChar();
				} while(isdigit(c));
				ungetChar(c);
				parser.yylval = new ParserVal(n);
				return Parser.NUMBER;
			} else if(isalpha(c)) {
				yytext = "";
				do {
					yytext += (char)c;
					c = getChar();
				} while(isalpha(c));
				ungetChar(c);
				if (yytext.equals("var")) return Parser.VAR;
				if (yytext.equals("if")) return Parser.IF;
				if (yytext.equals("else")) return Parser.ELSE;
				if (yytext.equals("return")) return Parser.RETURN;
				if (yytext.equals("while")) return Parser.WHILE;
				if (yytext.equals("for")) return Parser.FOR;
				if (yytext.equals("println")) return Parser.PRINTLN;
				parser.yylval = new ParserVal((Object)parser.makeSymbol(yytext));
				return Parser.SYMBOL;
				
			} else {
				throw new IOException("bad char '" + c + "'");
			}
		}
	}
}
