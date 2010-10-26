/*
 * tiny-C interpreter header file
 */

object InterP {
	var envp = 0
	val MAX_ENV = 100
	class Environment(var vr:SymbolC, var vl:Int)
	var env = Array[Environment](MAX_ENV)
	var funcReturnVal = 0

	def defineFunction(fsym:SymbolC, params:AST, body:AST) {
		fsym.func_params = params
		fsym.func_body = body
	}

	/*
	 * For environment
	 */
	def setValue(vr:SymbolC, vl:Int):Int = {
	    val i = envp - 1

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

		val nargs = executeFuncArgs(f.func_params, args)
		var vl = 0

		try {
			executeStatement(f.func_body)
		} catch {
		case Exception => vl = funcReturnVal
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
		case RETURN_STATEMENT(l, _) => executeReturn(l)
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
		while (statements != null) {
			executeStatement(getFirst(statements))
			statements = getNext(statements)
		}
		envp = envp_save
	}

	def executeIf(cond:AST, then_part:AST, else_part:AST) {
		if (executeExpr(cond)) {
			executeStatement(then_part)
		} else {
			executeStatement(else_part)
		}
	}

	def executeWhile(cond:AST, body:AST) {
		while (executeExpr(cond)) {
			executeStatement(body)
		}
	}

	def executeFor(init:AST, cond:AST, iter:AST, body:AST) {
	    // not implmented
	}

	def executeExpr(p:AST):Int = {
		p match {
		case NUM(v) => v
		case SYM(_) => getValue(getSymbol(p))
		case EQ_OP(l, r) => setValue(getSymbol(l), executeExpr(r))
		case PLUS_OP(l, r) => executeExpr(l) + executeExpr(r)
		case MINUS_OP(l, r) => executeExpr(l) - executeExpr(r)
		case MUL_OP(l, r) => executeExpr(l) * executeExpr(r)
		case LT_OP(l, r) => executeExpr(l) < executeExpr(r)
		case GT_OP(l, r) => executeExpr(l) > executeExpr(r)
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
		printf((char *)executeExpr(getNth(args,0)),
			executeExpr(getNth(args,1)))
		println()
	}

	/**
	 * global variable
	 */
	def declareVariable(vsym:SymbolC, init_value:AST) {
		if (init_value != null) {
			vsym.vl = executeExpr(init_value)
		}
	}

	/**
	 * Array
	 */
	def declareArray(a:SymbolC, size:AST) {
		a.vl = Array[Int](executeExpr(size))
	}

	def getArray(a:Array[Int], int index):Int = {
		a(index)
	}
	def setArray(a:Array[Int], index:Int, value:Int):Int = {
		a(index) = value
		value
	}

}
