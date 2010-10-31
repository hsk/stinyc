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

object Main extends Parser {
	val VAR_ARG = 0
	val VAR_LOCAL = 1
	val MAX_ENV = 100

	class Environment(var vr:SymbolC, var vr_kind:Int, var pos:Int)




	def main(args:Array[String]) {
		var r = new FileReader(args(0));
	    lexer = new CLex(r, this);
	    init();
	    yyparse()
	    return 0
	}


	var envp = 0
	var env = new Array[Environment](MAX_ENV)

	var label_counter = 0
	var local_var_pos = 0
	var tmp_counter = 0
	def init() {
		for (i <- 0 until MAX_ENV) {
			env(i) = new Environment(null,0,0)
		}
	}
	def compileStoreVar(vr:SymbolC, r:Int) {
		var i = envp - 1
		while(i >= 0){
			if (env(i).vr == vr){
				env(i).vr_kind match {
				case VAR_ARG => return genCode(STOREA(r, env(i).pos))
				case VAR_LOCAL => return genCode(STOREL(r, env(i).pos))
				}
			}
			i -= 1
		}
		throw new Exception("undefined variable\n")
	}

	def compileLoadVar(target:Int, vr:SymbolC) {
		var i = envp - 1
		while (i >= 0) {
			if (env(i).vr == vr){
				env(i).vr_kind match {
				case VAR_ARG => genCode(LOADA(target, env(i).pos)); return
				case VAR_LOCAL => genCode(LOADL(target, env(i).pos)); return
				}
			}
			i -= 1
		}
		throw new Exception("undefined variable\n")
	}

	override def defineFunction(fsym:stinyc.Ast#SymbolC, params:Ast#AST, body:stinyc.Ast#AST) {

		initGenCode()
		envp = 0
		var param_pos = 0
		local_var_pos = 0

		var ps = params.asInstanceOf[AST]
		while (ps != null) {
			env(envp).vr = getSymbol(getFirst(ps))
			env(envp).vr_kind = VAR_ARG
			env(envp).pos = param_pos
			param_pos += 1
			envp += 1
			ps = getNext(ps)
		}
		compileStatement(body.asInstanceOf[AST])
		genFuncCode(fsym.name, local_var_pos)
		envp = 0  // reset
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
		val envp_save = envp
		var lv = local_vars
		while (lv != null) {
			env(envp).vr = getSymbol(getFirst(lv))
			env(envp).vr_kind = VAR_LOCAL
			env(envp).pos = local_var_pos
			local_var_pos += 1
			envp += 1
			lv = getNext(lv)
		}
		var st = statements
		while(st != null) {
			compileStatement(getFirst(st))
			st = getNext(st)
		}
		envp = envp_save
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
		case _ => throw new Exception("println param error")
		}
	}

	/**
	 * global variable
	 */
	override def declareVariable(vsym:stinyc.Ast#SymbolC, init_value:Ast#AST) {
		// not implemented
	}

	/**
	 * Array
	 */
	override def declareArray(a:Ast#SymbolC, size:Ast#AST) {
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
				println("\tmovl\t" + TMP_OFF(i) + "(%%ebp)," + tmpRegName(rr))
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
				println("\tmovl\t%s,%d(%%ebp)\n",tmpRegName(reg),TMP_OFF(reg))
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
		println("\t.text")								// .text
		println("\t.align\t4")							// .align 4
		println("\t.globl\t%s\n", entry_name)			// .globl <name>
		println("\t.type\t%s,@function\n", entry_name)	// .type <name>,@function
		println("%s:\n", entry_name)					// <name>:
		println("\tpushl\t%%ebp\n")
		println("\tmovl\t%%esp,%%ebp\n")

		val frame_size = -LOCAL_VAR_OFF(n_local);
		val ret_lab = label_counter
		label_counter += 1

		println("\tsubl\t$" + frame_size + ",%%esp")
		println("\tmovl\t%%ebx,-4(%%ebp)")

		initTmpReg()

		codes foreach {case code@_ =>
			// debug println(code)
			code match {
			case LOADI(opd1, opd2) =>
				if(opd1 >= 0) {
					val r = getReg(opd1);
					println("\tmovl\t$" + opd2 + ","+ tmpRegName(r))
				}
			case LOADA(opd1, opd2) =>	// load arg
			    if(opd1 >= 0) {
					val r = getReg(opd1);
					println("\tmovl\t" + ARG_OFF(opd2) + "(%%ebp)," + tmpRegName(r));
				}
			case LOADL(opd1, opd2) =>	// load local
				if(opd1 >= 0) {
					val r = getReg(opd1)
					println("\tmovl\t" + LOCAL_VAR_OFF(opd2) + "(%%ebp)," + tmpRegName(r))
				}
			case STOREA(opd1, opd2) =>	// store arg
				val r = useReg(opd1)
				freeReg(r)
				println("\tmovl\t" + tmpRegName(r) + "," + ARG_OFF(opd2) + "(%%ebp)")
			case STOREL(opd1, opd2) =>	// store local
				val r = useReg(opd1)
				freeReg(r)
				println("\tmovl\t" + tmpRegName(r) + "," + LOCAL_VAR_OFF(opd2) + "(%%ebp)")
			case BEQ0(opd1, opd2) => // conditional branch
				val r = useReg(opd1)
				freeReg(r)
				println("\tcmpl\t$0," + tmpRegName(r))
				println("\tje\t.L" + opd2)
			case LABEL(opd1) =>
			    println(".L" + opd1 + ":")
			case JUMP(opd1) =>
			    println("\tjmp\t.L" + opd1)

			case CALL(opd1, opd2, opds) =>
				saveAllRegs()
				println("\tcall\t" + opds)
				if (opd1 >= 0) {
					assignReg(opd1, REG_AX)
					println("\tadd $" + (opd2 * 4) + ",%%esp")
				}
			case ARG(opd1) =>
				val r = useReg(opd1)
				freeReg(r)
				println("\tpushl " + tmpRegName(r))
			case RET(opd1) =>
				val r = useReg(opd1)
				freeReg(r)
				if (r != REG_AX) {
					println("\tmovl\t" + tmpRegName(r) + ",%%eax")
				}
				println("\tjmp .L" + ret_lab)

			case ADD(opd1, opd2, opd3) =>
				val r1 = useReg(opd2)
				val r2 = useReg(opd3)
				freeReg(r1)
				freeReg(r2)
				if (opd1 >= 0) {
					assignReg(opd1,r1)
					println("\taddl\t" + tmpRegName(r2) + "," + tmpRegName(r1))
				}
			case SUB(opd1, opd2, opd3) =>
				val r1 = useReg(opd2)
				val r2 = useReg(opd3)
				freeReg(r1)
				freeReg(r2)
				if (opd1 >= 0) {
					assignReg(opd1, r1)
					println("\tsubl\t" + tmpRegName(r2) + "," + tmpRegName(r1))
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
						println("\tmovl " + tmpRegName(r1) + "," + tmpRegName(REG_AX))
					}
					println("\timull\t" + tmpRegName(r2) + "," + tmpRegName(REG_AX))
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
					println("\tcmpl\t" + tmpRegName(r2) + "," + tmpRegName(r1))
					println("\tjl .L" + l1)
					println("\tmovl\t$0," + tmpRegName(r))
					println("\tjmp .L" + l2);
					println(".L" + l1 + ":\tmovl\t$1," + tmpRegName(r))
					print(".L" + l2 + ":")
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

					println("\tcmpl\t" + tmpRegName(r2) + "," + tmpRegName(r1))
					println("\tjg .L" + l1)
					println("\tmovl\t$0," + tmpRegName(r))
					println("\tjmp .L" + l2)
					println(".L" + l1 + ":\tmovl\t$1," + tmpRegName(r))
					print(".L" + l2 + ":")
				}
			case PRINTLN(opd1, opd2) =>
				val r = useReg(opd1); freeReg(r)
				println("\tpushl\t" + tmpRegName(r))
				println("\tpushl\t$.LC" + opd2)
				saveAllRegs()
				println("\tcall\tprintln")
				println("\taddl\t$8,%%esp")
			}
		}

		// return sequence
		println(".L" + ret_lab + ":\tmovl\t-4(%%ebp), %%ebx")
		println("\tleave")
		println("\tret")
	}

	def genString(s:String):Int = {
		val l = label_counter
		label_counter += 1
		println("\t.section\t.rodata")
		println(".LC" + l + ":")
		println("\t.string \"" + s + "\"")
		l
	}
}
