package stinyc;
import scala.collection.mutable.Stack
import java.io.FileReader

/*
 * tiny-C interpreter header file
 */

object InterP extends Parser {
	var envp = 0
	val MAX_ENV = 100
	case class Environment(var vr:SymbolC, var vl:Int)
	var env = new Array[Environment](MAX_ENV)
	var funcReturnVal = 0
	def init() {
		for(i <- 0 until MAX_ENV) {
			env(i) = Environment(null, 0)
		}
	}
	def readDefineFunction(fsym:stinyc.Ast#SymbolC, params:Ast#AST, body:stinyc.Ast#AST) {
		fsym.setFuncParams(params.asInstanceOf[Ast.AST])
		fsym.setFuncBody(body.asInstanceOf[Ast.AST])
	}

	/*
	 * For environment
	 */
	def setValue(vr:SymbolC, vl:Int):Int = {
	    var i = envp - 1

		while (i >= 0) {
			if(env(i).vr == vr) {
				env(i).vl = vl
				return vl
			}
			i -= 1
	    }
	    vr.vl = vl
	    return vl
	}

	def getValue(vr:SymbolC):Int = {
		var i = envp - 1
		while (i >= 0) {
			if (env(i).vr == vr) return env(i).vl
			i-= 1
		}
		return vr.vl
	}

	def executeCallFunc(f:SymbolC, args:AST):Int = {
		val nargs = executeFuncArgs(f.func_params.asInstanceOf[AST], args)
		var vl = 0
		try {
			executeStatement(f.func_body.asInstanceOf[AST])
		} catch {
		case _=> vl = funcReturnVal
		}
		envp -= nargs
		vl
	}

	private def executeFuncArgs(params:AST, args:AST):Int = {
		if (params == null) return 0
		val vl = executeExpr(getFirst(args))
		val vr = getSymbol(getFirst(params))
		val nargs = executeFuncArgs(getNext(params), getNext(args))
		env(envp).vr = vr
		env(envp).vl = vl
		envp += 1
		return nargs + 1
	}

	def executeReturn(expr:AST) {
		funcReturnVal = executeExpr(expr)
		throw new Exception("longjmp")
	}

	def executeStatement(p:AST) {
		p match {
		case BLOCK_STATEMENT(l, r) => executeBlock(l, r)
		case RETURN_STATEMENT(l) => executeReturn(l)
		case IF_STATEMENT(l, r) => executeIf(l, getNth(r, 0), getNth(r, 1))
		case WHILE_STATEMENT(l, r) => executeWhile(l, r)
		case FOR_STATEMENT(l, r) => executeFor(getNth(l, 0), getNth(l, 1), getNth(l, 2), r)
		case x => executeExpr(x)
	    }
	}

	def executeBlock(local_vars:AST, statements:AST) {
		val envp_save = envp
		var vars = local_vars
		while (vars != null) {
			val ep = envp
			envp += 1
			env(ep).vr = getSymbol(getFirst(vars))
			vars = getNext(vars)
		}
		var st = statements
		while (st != null) {
			executeStatement(getFirst(st))
			st = getNext(st)
		}
		envp = envp_save
	}

	def executeIf(cond:AST, then_part:AST, else_part:AST) {
		if (executeExpr(cond) != 0) {
			executeStatement(then_part)
		} else {
			executeStatement(else_part)
		}
	}

	def executeWhile(cond:AST, body:AST) {
		while (executeExpr(cond) != 0) {
			executeStatement(body)
		}
	}

	def executeFor(init:AST, cond:AST, iter:AST, body:AST) {
	    // not implmented
	}

	def executeExpr(p:AST):Int = {
		p match {
		case STR(s) => 0
		case NUM(v) => v
		case SYM(_) => getValue(getSymbol(p))
		case EQ_OP(l, r) => setValue(getSymbol(l), executeExpr(r))
		case PLUS_OP(l, r) => executeExpr(l) + executeExpr(r)
		case MINUS_OP(l, r) => executeExpr(l) - executeExpr(r)
		case MUL_OP(l, r) => executeExpr(l) * executeExpr(r)
		case LT_OP(l, r) => if (executeExpr(l) < executeExpr(r)) -1 else 0
		case GT_OP(l, r) => if (executeExpr(l) > executeExpr(r)) -1 else 0
		case GET_ARRAY_OP(l, r) => getArray(executeExpr(l),executeExpr(r))
		case SET_ARRAY_OP(l, r) => setArray(executeExpr(getNth(l, 0)),
				executeExpr(getNth(l, 1)),
				executeExpr(r))
		case CALL_OP(l, r) => executeCallFunc(getSymbol(l), r)
		case PRINTLN_OP(l) => printFunc(l); 0
		case null => 0
		case _ => throw new Exception("unknown operater/statement")
		}
	}

	private def printFunc(args:AST) {
		println(executeExpr(getNth(args,0)) + executeExpr(getNth(args,1)))
	}

	/**
	 * global variable
	 */
	def readDeclareVariable(vsym:stinyc.Ast#SymbolC, init_value:stinyc.Ast#AST) {
		if (init_value != null) {
			vsym.vl = executeExpr(init_value.asInstanceOf[AST])
		}
	}

	var arrays = new Stack[Array[Int]]()
	/**
	 * Array
	 */
	def readDeclareArray(a:stinyc.Ast#SymbolC, size:stinyc.Ast#AST) {
		a.vl = arrays.length
		arrays.push(new Array[Int](executeExpr(size.asInstanceOf[AST])))
		
	}

	def getArray(a:Int, index:Int):Int = {
		arrays(a)(index)
	}
	def setArray(a:Int, index:Int, value:Int):Int = {
		arrays(a)(index) = value
		value
	}

	def main(args:Array[String]) {
		init()
		var r = new FileReader(args(0));
	    lexer = new CLex(r, this);
		yyparse();
		var prg = yyval.obj.asInstanceOf[Program]
		readProgram(prg)
		// execute main
		println("execute main ...")
		val r2 = executeCallFunc(getSymbol(makeSymbol("main")), null)
		println("execute end ...")
		println( r2)
	}

	def readProgram(prg:Program) {
		prg match {
		case null =>
		case Program(p) => readExternalDefinitions(p)
		}
	}
	def readExternalDefinitions(l:List[ExternalDefinition]) {
		l match {
		case List() =>
		case DefineFunction(a,b,c)::xs => readDefineFunction(a, b, c); readExternalDefinitions(xs)
		case DeclareVariable(a,b)::xs => readDeclareVariable(a, b); readExternalDefinitions(xs)
		case DeclareArray(a,b)::xs => readDeclareArray(a, b); readExternalDefinitions(xs)
		}
	}
}
