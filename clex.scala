import java.io.InputStream

object CLex {
	val EOF = 255
	val STRING = 256
	val NUMBER = 257
	val VAR = 258
	val IF = 259
	val ELSE = 260
	val RETURN = 261
	val WHILE = 262
	val FOR = 263
	val PRINTLN = 264
	val SYMBOL = 265

	var yyval:AST.AST = null
	var yytext:String = ""

	private var buf:Option[Char] = None
	private var in:InputStream = null

	def setInputStream(in:InputStream) {
		CLex.in = in
	}

	// for debug
	private def getChar():Char = {
		buf match {
		case None => in.read().asInstanceOf[Char]
		case Some(a) => buf = None; a
		}
	}

	private def ungetChar(c:Char) {
		buf = Some(c)
	}

	private def isspace(c:Char):Boolean = {
		c match {
		case ' ' | '\t' | '\r' | '\n' => true
		case _ => false
		}
	}

	private def isdigit(c:Char):Boolean = {
		if (c >= '0' && c <= '9') true else false
	}

	private def isalpha(c:Char):Boolean = {
		if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') true else false
	}
	def yylex():Int = {
		var c:Char = ' '
		while (isspace(c)) {
			c = getChar()
		}

	    c match {
	    case '+' | '-' | '*' | '>' | '<' | '(' | ')' | '{' | '}' | ']' | '[' | ';' | ',' | '=' | EOF => c
	    case '"' => yytext = ""
	    	var flg = true
			while (flg) {
				val c = getChar()
				if (c == '"') {
					flg = false
				} else {
					yytext += c
				}
			}
			yyval = AST.STR(yytext)
			STRING
		case _ if (isdigit(c)) =>
			var n = 0
			do {
			    n = n * 10 + c - '0'
			    c = getChar()
			} while(isdigit(c))
			ungetChar(c)
			yyval = AST.NUM(n)
			NUMBER
		case _  if(isalpha(c)) =>
			yytext = ""
			do {
				yytext += c
				c = getChar()
			} while(isalpha(c))
			ungetChar(c)
			yytext match {
			case "var" => VAR
			case "if" => IF
			case "else" => ELSE
			case "return" => RETURN
			case "while" => WHILE
			case "for" => FOR
			case "println" => PRINTLN
			case _ => yyval = AST.makeSymbol(yytext); SYMBOL
			}
		case _ => throw new Exception("bad char '" + c + "'")
		}
	}
}
