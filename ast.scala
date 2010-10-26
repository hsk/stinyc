object AST {
	case class SymbolC(var name:String) {
		var vl:Int = 0
		var func_params:AST = null
		var func_body:AST = null
	}

	sealed abstract class AST
	case class LIST(var l:AST,var r:AST) extends AST
	case class NUM(vl:Int) extends AST
	case class STR(vl:String) extends AST
	case class SYM(sym:SymbolC) extends AST
	case class EQ_OP(l:AST,r:AST) extends AST
	case class PLUS_OP(l:AST,r:AST) extends AST
	case class MINUS_OP(l:AST,r:AST) extends AST
	case class MUL_OP(l:AST,r:AST) extends AST
	case class LT_OP(l:AST,r:AST) extends AST
	case class GT_OP(l:AST,r:AST) extends AST
	case class GET_ARRAY_OP(l:AST,r:AST) extends AST
	case class SET_ARRAY_OP(l:AST,r:AST) extends AST
	case class CALL_OP(l:AST,r:AST) extends AST
	case class PRINTLN_OP(l:AST,r:AST) extends AST
	case class IF_STATEMENT(l:AST,r:AST) extends AST
	case class BLOCK_STATEMENT(l:AST,r:AST) extends AST
	case class RETURN_STATEMENT(l:AST,r:AST) extends AST
	case class WHILE_STATEMENT(l:AST,r:AST) extends AST
	case class FOR_STATEMENT(l:AST,r:AST) extends AST

	def getFirst(p:AST):AST = getNth(p, 0)
	def makeList1(x1:AST):AST = LIST(x1,null)
	def makeList2(x1:AST, x2:AST):AST = LIST(x1,LIST(x2,null))
	def makeList3(x1:AST, x2:AST, x3:AST):AST = LIST(x1,LIST(x2,LIST(x3,null)))

	def getNth(p:AST, n:Int):AST = {
		p match {
		case LIST(l, r) => if (n > 0) getNth(r, n - 1) else l
		case _ => throw new Exception("bad access to list")
		}
	}

	def addLast(l:AST, p:AST):AST = {
		l match {
		case null => LIST(p, null)
		case l@LIST(x, null) => l.r = LIST(p, null); l
		case LIST(x, xs) => addLast(xs, p); l
		case _ => throw new Exception("bad access to list")
		}
	}

	def getNext(p:AST):AST = {
		p match {
		case LIST(l,r) => r
		case _ => throw new Exception("bad access to list")
		}
	}

	def makeSymbol(name:String):AST = SYM(SymbolC(name))

	def getSymbol(p:AST):SymbolC = {
		p match {
		case SYM(s) => s
		case _ => throw new Exception("bad access to symbol")
		}
	}
}
