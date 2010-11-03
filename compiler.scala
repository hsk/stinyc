/*
 * tiny-c compiler
 */
package stinyc
import scala.collection.mutable.Stack
import java.io.FileReader

/*
 * register machine intermediate opcode
 */
abstract class IR()
case class LOADI(r:Int, n:Int) extends IR // load int
case class LOADA(r:Int, n:Int) extends IR // load arg
case class LOADL(r:Int, n:Int) extends IR // load local var
case class STOREA(r:Int, n:Int) extends IR // store arg
case class STOREL(r:Int, n:Int) extends IR // store local var
case class ADD(t:Int, r1:Int, r2:Int) extends IR
case class SUB(t:Int, r1:Int, r2:Int) extends IR
case class MUL(t:Int, r1:Int, r2:Int) extends IR
case class GT(t:Int, r1:Int, r2:Int) extends IR
case class LT(t:Int, r1:Int, r2:Int) extends IR
case class BEQ0(t:Int, l:Int) extends IR // branch if eq 0
case class JUMP(l:Int) extends IR
case class ARG(r:Int) extends IR // set Argument
case class CALL(t:Int, n:Int, f:String) extends IR
case class RET(r:Int) extends IR // return
case class PRINTLN(r:Int, l:Int) extends IR // println function
case class LABEL(l:Int) extends IR // label
/*
case class LOADS() extends IR  // load string label

case class LOADADDR() extends IR 
case class LOAD() extends IR 
case class STORE() extends IR 
*/

object Compiler extends Parser {

	sealed class EnvKind()
	case class EnvVarARG() extends EnvKind
	case class EnvVarLocal() extends EnvKind
	
	case class Environment(var vr:SymbolC, var vr_kind:EnvKind, var pos:Int)

	def main(args:Array[String]) {
		val r = new FileReader(args(0));
		lexer = new CLex(r, this);
		yyparse()
		var prg = yyval.obj.asInstanceOf[Program]
		println(prg)
		compileProgram(prg)
	}

	def compileProgram(prg:Program) {
		prg match {
		case null =>
		case Program(p) => compileExternalDefinitions(p)
		}
	}
	def compileExternalDefinitions(l:List[ExternalDefinition]) {
		l match {
		case List() =>
		case DefineFunction(a,b,c)::xs => compileDefineFunction(a, b, c); compileExternalDefinitions(xs)
		case DeclareVariable(a,b)::xs => compileDeclareVariable(a, b); compileExternalDefinitions(xs)
		case DeclareArray(a,b)::xs => compileDeclareArray(a, b); compileExternalDefinitions(xs)
		}
	}

	var env = List[Environment]()
	
	var label_counter = 0
	var local_var_pos = 0
	var tmp_counter = 0

	def compileStoreVar(vr:SymbolC, r:Int) {
		def t(env:List[Environment]) {
			env match {
			case List() => throw new Exception("undefined variable\n")
			case x::xs =>
				if (x.vr == vr) {
					x.vr_kind match {
					case EnvVarARG() => genCode(STOREA(r, x.pos))
					case EnvVarLocal() => genCode(STOREL(r, x.pos))
					}
				} else t(xs)
			}
		}
		t(env)
	}

	def compileLoadVar(target:Int, vr:SymbolC) {
		def t(env:List[Environment]) {
			env match {
			case List() => throw new Exception("undefined variable")
			case x::xs =>
				if (x.vr == vr) {
					x.vr_kind match {
					case EnvVarARG() => genCode(LOADA(target, x.pos))
					case EnvVarLocal() => genCode(LOADL(target, x.pos))
					}
				} else t(xs)
			}
		}
		t(env)
	}

	def compileDefineFunction(fsym:stinyc.Ast#SymbolC, params:Ast#AST, body:stinyc.Ast#AST) {

		initGenCode()
		local_var_pos = 0
		
		def getEnv(ps:AST, param_pos:Int, env:List[Environment]):List[Environment] = {
			if (ps == null) env
			else getEnv(
				getNext(ps),
				param_pos + 1,
				Environment(getSymbol(getFirst(ps)), EnvVarARG(), param_pos)::env
			)
		}
		env = getEnv(params.asInstanceOf[AST], 0, List[Environment]())
		compileStatement(body.asInstanceOf[AST])
		genFuncCode(fsym.name, local_var_pos)
		env = List[Environment]() // reset
	}

	def compileStatement(p:AST) {
	    p match {
	    case null =>
	    case BLOCK_STATEMENT(l, r) => compileBlock(l, r)
	    case RETURN_STATEMENT(l) => compileReturn(l)
	    case IF_STATEMENT(l, r) => compileIf(l, getNth(r, 0), getNth(r, 1))
	    case WHILE_STATEMENT(l, r) => compileWhile(l, r)
	    case FOR_STATEMENT(l, r) => compileFor(getNth(l, 0), getNth(l, 1), getNth(l, 2), r)
		case _ => compileExpr(-1, p)
	    }
	}

	def compileBlock(local_vars:AST, statements:AST) {
		val env_save = env
		var lv = local_vars
		while (lv != null) {
			env = Environment(getSymbol(getFirst(lv)), EnvVarLocal(), local_var_pos)::env
			local_var_pos += 1
			lv = getNext(lv)
		}
		var st = statements
		while(st != null) {
			compileStatement(getFirst(st))
			st = getNext(st)
		}
		env = env_save
	}

	def compileReturn(expr:AST) {
		if (expr != null) {
			val r = tmp_counter
			tmp_counter += 1
			compileExpr(r, expr)
			genCode(RET(r))
		} else {
			genCode(RET(-1))
		}
	}

	def compileCallFunc(target:Int, f:SymbolC, args:AST) {
		val narg = compileArgs(args)
		genCode(CALL(target, narg, f.name))
	}

	def compileArgs(args:AST):Int = {
		if (args != null) {
			val n = compileArgs(getNext(args))
			val r = tmp_counter
			tmp_counter += 1
			compileExpr(r, getFirst(args))
			genCode(ARG(r))
			n + 1
		} else {
			0
		}
	}

	def compileIf(cond:AST, then_part:AST, else_part:AST) {
		val r = tmp_counter
		tmp_counter += 1
		compileExpr(r, cond)
		val l1 = label_counter
		label_counter += 1
		genCode(BEQ0(r, l1))
		compileStatement(then_part)
		if (else_part != null) {
			val l2 = label_counter
			label_counter += 1
			genCode(JUMP(l2))
			genCode(LABEL(l1))
			compileStatement(else_part)
			genCode(LABEL(l2))
		} else {
			genCode(LABEL(l1))
		}
	}

	def compileWhile(cond:AST, body:AST) {
		val l1 = label_counter
		label_counter += 1
		val l2 = label_counter
		label_counter += 1
		val r = tmp_counter
		tmp_counter += 1

		genCode(LABEL(l1))
		compileExpr(r, cond)
		genCode(BEQ0(r, l2))
		compileStatement(body)
		genCode(JUMP(l1))
		genCode(LABEL(l2))
	}

	def compileFor(init:AST, cond:AST, iter:AST, body:AST) {
	    // not implemented
	}

	def compileExpr(target:Int, p:AST) {
	    p match {
	    case null =>
	    case NUM(v) => genCode(LOADI(target, v))
	    case SYM(_) => compileLoadVar(target,getSymbol(p));
	    case EQ_OP(l, r) =>
			if(target != -1) throw new Exception("assign has no value")
			val r1 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, r)
			compileStoreVar(getSymbol(l), r1)
	    case PLUS_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(ADD(target, r1, r2))

	    case MINUS_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(SUB(target, r1, r2))

	    case MUL_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(MUL(target, r1, r2))

	    case LT_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(LT(target, r1, r2))

	    case GT_OP(l, r) =>
			val r1 = tmp_counter
			tmp_counter += 1
			val r2 = tmp_counter
			tmp_counter += 1
			compileExpr(r1, l)
			compileExpr(r2, r)
			genCode(GT(target, r1, r2))

	    case CALL_OP(l, r) => compileCallFunc(target,getSymbol(l), r)
	    case PRINTLN_OP(l) =>
			if(target != -1) throw new Exception("println has no value")
			printFunc(l)

	    case GET_ARRAY_OP(_,_) =>
			// not implemented
	    case SET_ARRAY_OP(_,_) =>
			// not implemented
		case _ => throw new Exception("unknown operater/statement")
	    }
	}

	private def printFunc(args:AST) {
		args match {
		case LIST(STR(a),b) =>
			val l = genString(a)
			val r = tmp_counter
			tmp_counter += 1
			compileExpr(r, getNth(args, 1))
			genCode(PRINTLN(r, l))
		case LIST(SYM(SymbolC(a)),b) =>
			val l = genString(a)
			val r = tmp_counter
			tmp_counter += 1
			compileExpr(r, getNth(args, 1))
			genCode(PRINTLN(r, l))
		case _ => throw new Exception("println param error" + args)
		}
	}

	/**
	 * global variable
	 */
	def compileDeclareVariable(vsym:stinyc.Ast#SymbolC, init_value:Ast#AST) {
		// not implemented
	}

	/**
	 * Array
	 */
	def compileDeclareArray(a:Ast#SymbolC, size:Ast#AST) {
		// not implemented
	}

	var codes = new Stack[IR]()

	def initGenCode() {
		codes = new Stack[IR]()
	}

	def genCode(c:IR) {
		codes.push(c)
	}

	/* 
	 *  code generator for x86
	 */ 

	val N_REG = 4
	val N_SAVE = 4

	def TMP_OFF(i:Int):Int = -((i+1)+1)*4
	def LOCAL_VAR_OFF(i:Int):Int = -(N_SAVE+1+(i+1))*4
	def ARG_OFF(i:Int):Int = ((i)+2)*4

	val REG_AX = 0
	val REG_BX = 1
	val REG_CX = 2
	val REG_DX = 3

	val tmpRegName = Array("%eax", "%ebx", "%ecx", "%edx")
	val tmpRegState = Array(-1,-1,-1,-1)
	val tmpRegSave = Array(-1,-1,-1,-1)

	def initTmpReg() {
		for(i <- 0 until N_REG) {
			tmpRegState(i) = -1
		}
		for(i <- 0 until N_SAVE) {
			tmpRegSave(i) = -1
		}
	}

	/**
	 * getReg: get free register
	 */
	def getReg(r:Int):Int = {
		for (i <- 0 until N_REG) {
			if(tmpRegState(i) < 0){
				tmpRegState(i) = r
				return i
			}
		}
		throw new Exception("no temp reg")
	}

	/**
	 * assign r to reg
	 */
	def assignReg(r:Int, reg:Int) {
		if (tmpRegState(reg) == r) return
		saveReg(reg)
		tmpRegState(reg) = r
	}

	/**
	 * load r into reg
	 */
	def useReg(r:Int):Int = {
	    for (i <- 0 until N_REG) {
			if(tmpRegState(i) == r) return i
		}
		// not found in register, then restore from save area.
		for (i <- 0 until N_SAVE) {
			if (tmpRegSave(i) == r) {
				val rr = getReg(r)
				tmpRegSave(i) = -1
				// load into regsiter
				println("\tmovl\t" + TMP_OFF(i) + "(%ebp)," + tmpRegName(rr))
				return rr
			}
		}
		throw new Exception("reg is not found")
	}

	def freeReg(reg:Int) {
		tmpRegState(reg) = -1
	}

	def saveReg(reg:Int) {
		if(tmpRegState(reg) < 0) return
		for (i <- 0 until N_SAVE) {
			if (tmpRegSave(i) < 0) {
				println("\tmovl\t" + tmpRegName(reg) + "," + TMP_OFF(reg) + "(%ebp)")
				tmpRegSave(i) = tmpRegState(reg)
				tmpRegState(reg) = -1
				return
			}
		}
		throw new Exception("no temp save")
	}

	def saveAllRegs() {
	    for(i <- 0 until N_REG) saveReg(i)
	}

	/*
	 * Code generation
	 */
//	extern int label_counter;

	def genFuncCode(entry_name:String, n_local:Int) {

		// function header
		asm._text()
		asm._align(4)
		asm._globl(entry_name)
		asm._type(entry_name, "@function")
		asm.label(entry_name)
		
		asm.pushl("%ebp")
		asm.movl("%esp", "%ebp")

		val frame_size = -LOCAL_VAR_OFF(n_local)
		val ret_lab = label_counter
		label_counter += 1

		asm.subl("$" + frame_size, "%esp")
		asm.movl("%ebx", "-4(%ebp)")

		initTmpReg()

		codes foreach {
		case LOADI(opd1, opd2) =>
			if(opd1 >= 0) {
				val r = getReg(opd1)
				asm.movl("$" + opd2, tmpRegName(r))
			}
		case LOADA(opd1, opd2) =>	// load arg
		    if(opd1 >= 0) {
				val r = getReg(opd1)
				asm.movl(ARG_OFF(opd2) + "(%ebp)", tmpRegName(r))
			}
		case LOADL(opd1, opd2) =>	// load local
			if(opd1 >= 0) {
				val r = getReg(opd1)
				asm.movl(LOCAL_VAR_OFF(opd2) + "(%ebp)", tmpRegName(r))
			}
		case STOREA(opd1, opd2) =>	// store arg
			val r = useReg(opd1)
			freeReg(r)
			asm.movl(tmpRegName(r), ARG_OFF(opd2) + "(%ebp)")
		case STOREL(opd1, opd2) =>	// store local
			val r = useReg(opd1)
			freeReg(r)
			asm.movl(tmpRegName(r), LOCAL_VAR_OFF(opd2) + "(%ebp)")
		case BEQ0(opd1, opd2) => // conditional branch
			val r = useReg(opd1)
			freeReg(r)
			asm.cmpl("$0", tmpRegName(r))
			asm.je(".L" + opd2)
		case LABEL(opd1) =>
			asm.label(".L" + opd1)
		case JUMP(opd1) =>
			asm.jmp(".L" + opd1)

		case CALL(opd1, opd2, opds) =>
			saveAllRegs()
			asm.call(opds)
			if (opd1 >= 0) {
				assignReg(opd1, REG_AX)
				asm.add("$" + (opd2 * 4), "%esp")
			}
		case ARG(opd1) =>
			val r = useReg(opd1)
			freeReg(r)
			asm.pushl(tmpRegName(r))
		case RET(opd1) =>
			val r = useReg(opd1)
			freeReg(r)
			if (r != REG_AX) {
				asm.movl(tmpRegName(r), "%eax")
			}
			asm.jmp(".L" + ret_lab)

		case ADD(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				assignReg(opd1,r1)
				asm.addl(tmpRegName(r2), tmpRegName(r1))
			}
		case SUB(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				assignReg(opd1, r1)
				asm.subl(tmpRegName(r2), tmpRegName(r1))
			}
		case MUL(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				assignReg(opd1, REG_AX)
				saveReg(REG_DX)
				if(r1 != REG_AX) {
					asm.movl(tmpRegName(r1), tmpRegName(REG_AX))
				}
				asm.imull(tmpRegName(r2), tmpRegName(REG_AX))
			}
		case LT(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
			freeReg(r1)
			freeReg(r2)
			if (opd1 >= 0) {
				val r = getReg(opd1)
				val l1 = label_counter
				label_counter += 1
				val l2 = label_counter
				label_counter += 1
				asm.cmpl(tmpRegName(r2), tmpRegName(r1))
				asm.jl(".L" + l1)
				asm.movl("$0", tmpRegName(r))
				asm.jmp(".L" + l2)
				asm.label(".L" + l1)
				asm.movl("$1", tmpRegName(r))
				asm.label(".L" + l2)
			}
		case GT(opd1, opd2, opd3) =>
			val r1 = useReg(opd2)
			val r2 = useReg(opd3)
		    freeReg(r1)
		    freeReg(r2)
		    if (opd1 >= 0) {
				val r = getReg(opd1)
				val l1 = label_counter
				label_counter += 1
				val l2 = label_counter
				label_counter += 1

				asm.cmpl(tmpRegName(r2), tmpRegName(r1))
				asm.jg(".L" + l1)
				asm.movl("$0", tmpRegName(r))
				asm.jmp(".L" + l2)
				asm.label(".L" + l1)
				asm.movl("$1", tmpRegName(r))
				asm.label(".L" + l2)
			}
		case PRINTLN(opd1, opd2) =>
			val r = useReg(opd1); freeReg(r)
			asm.pushl(tmpRegName(r))
			asm.pushl("$.LC" + opd2)
			saveAllRegs()
			asm.call("println")
			asm.addl("$8", "%esp")
		}

		// return sequence
		asm.label(".L" + ret_lab)
		asm.movl("-4(%ebp)", "%ebx")
		asm.leave()
		asm.ret()
	}

	def genString(s:String):Int = {
		val l = label_counter
		label_counter += 1
		asm._section(".rodata")
		asm.label(".LC" + l)
		asm._string(s)
		l
	}
}

object asm {

	/**
	 * テキスト
	 */
	def _text() {
		println("\t.text")
	}
	/**
	 * アライン
	 */
	def _align(r1:Int) {
		println("\t.align\t" + r1)
	}
	/**
	 * グローバル
	 */
	def _globl(r1:String) {
		println("\t.globl\t"+ r1)
	}
	/**
	 * タイプ
	 */
	def _type(r1:String, r2:String) {
		println("\t.type\t" + r1 + "," + r2)
	}
	/**
	 * セクション
	 */
	def _section(r1:String) {
		println("\t.section\t" + r1)
	}
	/**
	 * 文字列
	 */
	def _string(r1:String) {
		println("\t.string \"" + r1 + "\"")
	}

	/**
	 * ラベル
	 */
	def label(r1:String) {
		println(r1 + ":")
	}
	/**
	 * スタックにPUSH
	 */
	def pushl(r1:String) {
		println("\tpushl\t" + r1)
	}
	/**
	 * 掛け算
	 */
	def movl(r1:String, r2:String) {
		println("\tmovl\t" + r1 + "," + r2)
	}
	/**
	 * 足し算
	 */
	def add(r1:String, r2:String) {
		println("\tadd " + r1 + "," + r2)
	}
	/**
	 * 書け算
	 */
	def imull(r1:String, r2:String) {
		println("\timull\t" + r1 + "," + r2)
	}
	/**
	 * 足し算
	 */
	def addl(r1:String, r2:String) {
		println("\taddl\t" + r1 + "," + r2)
	}
	/**
	 * 引き算
	 */
	def subl(r1:String, r2:String) {
		println("\tsubl\t" + r1 + "," + r2)
	}
	/**
	 * 比較
	 */
	def cmpl(r1:String, r2:String) {
		println("\tcmpl\t" + r1 + "," + r2)
	}
	/**
	 * equalだったらjump
	 */
	def je(r1:String) {
		println("\tje\t" + r1)
	}
	/**
	 * 小さかったらjump
	 */
	def jl(r1:String) {
		println("\tjl\t" + r1)
	}
	/**
	 * 大きかったらjump
	 */
	def jg(r1:String) {
		println("\tjg\t" + r1)
	}
	/**
	 * 無条件jump
	 */
	def jmp(r1:String) {
		println("\tjmp\t" + r1)
	}

	/**
	 * 関数呼び出し
	 */
	def call(opds:String) {
		println("\tcall\t" + opds)
	}

	/**
	 * 関数終了処理
	 */
	def leave() {
		println("\tleave")
	}

	/**
	 * 関数リターン
	 */
	def ret() {
		println("\tret")
	}
}

