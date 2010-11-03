package stinyc
import java.util.Scanner
import java.io.File

object Compiler2 extends Ast {
	def main(args:Array[String]) {
		val src = new Scanner(new File(args(0))).useDelimiter("\\Z").next()

		val st = compact.main.parse(src)
		println(st)
		val ast = program(st)
		println(ast)
		Compiler.compileProgram(ast.asInstanceOf[Compiler.Program])
	}

	def program(st:Any):Program = {
		Program(externalDefinitions(st))
	}

	def externalDefinitions(st:Any):List[ExternalDefinition] = {
		st match {
		case ( x, "@", xs) => externalDefinition(x) :: externalDefinitions(xs)
		case x => List(externalDefinition(x))
		}
	}

	def externalDefinition(st:Any):ExternalDefinition = {
		st match {
		case ("def",(a,"=",("fun","(",p,")", b))) => DefineFunction(symbol(a), parameterList(p), block(b))
		case ("var",(a,"=",b)) => DeclareVariable(symbol(a), expr(b))
		case ("var", (a,"[",b,"]")) => DeclareArray(symbol(a), expr(b))
		case ("var", a) => DeclareVariable(symbol(a),null)
		case _ => throw new Exception("syntax error")
		}
	}

	def symbol(st:Any):SymbolC = {
		st match {
		case a:String => getSymbol(makeSymbol(a))
		case _ => throw new Exception("syntax error")
		}
	}

	def parameterList(st:Any):AST = {
		st match {
		case ("(", "void", ")") => null
		case ("(", s, ")") => symbolList(s)
		case "void" => null
		case _ => throw new Exception("syntax error '" + st + "'")
		}
	}

	def block(st:Any):BLOCK_STATEMENT = {
		st match {
		case ("{", l, "}") =>
			var (l1, st2) = localVars(l)
			var l2 = statements(st2)
			BLOCK_STATEMENT(l1, l2)
		case _ => throw new Exception("syntax error")
		}
	}

	def localVars(st:Any):(AST, Any) = {
		st match {
		case (("var", s),"@", b) => (symbolList(s), b)
		case x => (null, x)
		}
	}

	def symbolList(st:Any):AST = {
		st match {
		case a:String => makeList1(SYM(symbol(a)))
		case (a, ",", b) => LIST(SYM(symbol(a)), symbolList(b))
		case _ => throw new Exception("syntax error")
		}
	}

	def statements(st:Any):AST = {
		st match {
		case (a, "@", b) => LIST(statement(a),statements(b))
		case a => makeList1(statement(a))
		}
	}

	def statement(st:Any):AST = {
		st match {
		case ("{", a, "}") => statements(a)
		case ("if", "(", a, ")", (b, "else", c)) => IF_STATEMENT(expr(a),makeList2(block(b), block(c)))
		case ("if", "(", a, ")", b) => IF_STATEMENT(expr(a),makeList2(block(b),null))
		case ("return", ";") => RETURN_STATEMENT(null)
		case ("return", a) => RETURN_STATEMENT(expr(a))
		case ("while", "(", a, ")", b) => WHILE_STATEMENT(expr(a), block(b))
		case ("for", "(", a, ")", b1) =>
			a match {
			case (((a,";"),"@",(b,";")),"@",c) => FOR_STATEMENT(makeList3(expr(a),expr(b),expr(c)),block(b1))
			}
		case (a,";") => statement(a)
		case a => expr(a)
		}
	}
	
	def expr(st:Any):AST = {
		st match {
		case ((a,"[",b,"]"),"=",c) => SET_ARRAY_OP(makeList2(SYM(symbol(a)),expr(b)),expr(c))
		case (a,"=",b) => EQ_OP(SYM(symbol(a)),expr(b))
		case (a,"+",b) => PLUS_OP(expr(a),expr(b))
		case (a,"-",b) => MINUS_OP(expr(a),expr(b))
		case (a,"*",b) => MUL_OP(expr(a),expr(b))
		case (a,"<",b) => LT_OP(expr(a),expr(b))
		case (a,">",b) => GT_OP(expr(a),expr(b))
		case "void"=> null
		case a:String => SYM(symbol(a))
		case a:Int => NUM(a)
//		case a:String => STR(a)
		case (a,"[",b,"]") => GET_ARRAY_OP(SYM(symbol(a)), expr(b))
		case ("println","(",b,")") => PRINTLN_OP(argList(b))
		case (a,"(","void",")") => CALL_OP(SYM(symbol(a)), null)
		case (a,"(",b,")") => CALL_OP(SYM(symbol(a)), argList(b))
		case (a,"@",b) => statement(a); statement(b)
		}
	}
	
	def argList(st:Any):AST = {
		st match {
		case (a,",",b) => addLast(argList(a),SYM(symbol(b)))
		case a => makeList1(SYM(symbol(a)))
		}
	}

}
