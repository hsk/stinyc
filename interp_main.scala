import AST
import interp
object interp_main {
	def main(argv:Array[String]) {
		// execute main
		println("execute main ...")
		r = executeCallFunc(Symbol(main), null)
		println("execute end ...")
		return r
	}
}
