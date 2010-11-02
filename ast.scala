package stinyc
import scala.collection.mutable.Map

object Ast extends Ast
class Ast {
	case class SymbolC(var name:String) {
		var vl:Int = 0
		var array:Array[Int] = null
		var func_params:Ast.AST = null
		var func_body:Ast.AST = null
		def setFuncParams(a:Ast.AST){func_params = a}
		def setFuncBody(a:Ast.AST){func_body = a}
	}

	case class Program(l:List[ExternalDefinition])

	sealed abstract class ExternalDefinition
	case class DeclareArray(sym:SymbolC,o:AST) extends ExternalDefinition
	case class DeclareVariable(sym:SymbolC,o:AST) extends ExternalDefinition
	case class DefineFunction(sym:SymbolC,o:AST, o2:AST) extends ExternalDefinition

	def list(p:ExternalDefinition):List[ExternalDefinition] = List(p)
	def addList(p:ExternalDefinition,l:List[ExternalDefinition]):List[ExternalDefinition] = p::l
	def reverse(l:List[ExternalDefinition]):List[ExternalDefinition] = l.reverse
	def program(l:List[ExternalDefinition]):Program = Program(l)
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
	case class PRINTLN_OP(l:AST) extends AST
	case class IF_STATEMENT(l:AST,r:AST) extends AST
	case class BLOCK_STATEMENT(l:AST,r:AST) extends AST
	case class RETURN_STATEMENT(l:AST) extends AST
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
	val symbolTable = Map[String, SymbolC]()
	def makeSymbol(name:String):AST = {
		if (symbolTable.contains(name)) {
			SYM(symbolTable(name))
		}else {
			val sym = SymbolC(name)
			symbolTable += ( name -> sym )
			SYM(sym)
		}
	}
	def getSymbol(p:AST):SymbolC = {
		p match {
		case SYM(s) => s
		case _ => throw new Exception("bad access to symbol" + p)
		}
	}
	
}
