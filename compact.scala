/*
 * Copyright (c) 2010 h_sakurai, freecluster.net. All rights reserved.
 *
 * compact.scala
 * Functional Top Down Operator Precedence Compact eXPression parser
 */
package compact
import scala.util.matching.Regex
import scala.collection.mutable.{HashMap,Stack}

object main {

	def main(argv:Array[String]) {
		println(parse(argv(0)))
	}
	// c like expression reader
	def parse(src:String) = {
		var str = src.replace("\r\n", "")
		var connectf = true
		val tokens = for(i <- Stream.from(0)) yield {
			def t ():Any = {
				val num = """^([0-9]+)(.*$)""".r
				val sym = """^'([^-\+*\/\(\)\[\]\{\}\s\=\;\:<,]+)(.*$)""".r
				val ptn = """^([-\+*\/\=<]+|[\:\(\)\[\]\{\},]|[^-\+*\/\(\)\[\]\{\}\s\=\;\:<,]+|$)(.*$)""".r
				val eol = """^(;)(.*$)""".r
				val spc1 = """^[\s]*[\r\n]+[\s]*(.*$)""".r
				val spc = """^[\s]+(.*$)""".r
				val eof = """^[\s]*($)""".r
				str match {
				case eol(a,e) => str = e; connectf = false; a
				case eof(e) => str = ""; Nil
				case sym(a,e) => str = e; Symbol(a)
				case num(a,e) => str = e; a.toInt
				case ptn(a,e) => str = e; a
				case spc1(e) => str = e; connectf = false; t()
				case spc(e) => str = e; t()
				case _ => throw new Error("syntax error unexpected stream '"+str.substring(0,10)+"...'")
				}
			}
			connectf = true
			t()
		}
		val eox:(Any => Int) = {
			case ")" => 1
			case "}" => 1
			case "]" => 1
			case Nil => 1
			case _   => -1
		}
		val infixs:(Any => Int) = {
			case "*" => 20
			case "/" => 20
			case "-" => 10
			case "+" => 10
			case "<=" => 6
			case "<" => 6
			case ">=" => 6
			case ">" => 6
			case "==" => 5
			case "," => 2
			case "else" => 3
			case _	 => -1
		}

		val infixrs:(Any => Int) = {
			case "=" => 5
			case _	 => -1
		}

		val prefixs:(Any => Int) = {
			case "def" => 1
			case "var" => 1
			case "-" => 100
			case _	 => -1
		}

		val postfixs:(Any => Int) = {
			case "++" => 100
			case ";" => 0
			case _	 => -1
		}

		val parens:(Any => Int) = {
			case "(" => 100
			case "{" => 100
			case "[" => 100
			case _	 => -1
		}

		val endParens:(Any => Any) = {
			case "(" => ")"
			case "{" => "}"
			case "[" => "]"
			case _ => Nil
		}

		val sts:( (Any, Any) => Int ) = {
			case ("if","(") => 2
			case ("fun","(") => 2
			case ("mac","(") => 2
			case ("while","(") => 2
			case _ => -1
		}

		sealed case class AS(a:Any, s:Stream[Any])

		def eat(t:Any)(as:AS):AS = as.s match {
			case cons(x,xs) if(x==t) => AS(x, xs)
			case _ => throw new Error("error expected "+t+" but found "+as.s )
		}
		val cons = Stream.cons
		def exp(p:Int)(as:AS):AS = {
			//println("exp("+p+")("+as+")");
			as match {
			case AS(null, cons(x,xs)) if(eox(x)>0) => AS("void",cons(x,xs))
			case AS(null, cons(p1, xs)) if (p < parens(p1)) =>
				val AS(y, ys)  = exp(0)(AS(null, xs))
				val AS(p2, zs) = eat(endParens(p1))(AS(null, ys))
				exp(p)(AS((p1,y,p2),zs))
			case AS(null, cons(op, xs)) if (p < prefixs(op)) =>
				val AS(y, ys) = exp(prefixs(op))(AS(null, xs))
				exp(p)(AS((op,y),ys))
			case AS(null, cons(x, xs)) => exp(p)(AS(x, xs))
			case AS(x, cons(p1, xs)) if (0 < sts(x,p1)) =>
				val AS(y, ys)  = exp(0)(AS(null, xs))
				val AS(p2, zs) = eat(endParens(p1))(AS(null, ys))
				val AS(w, ws) = exp(sts(x,p1))(AS(null, zs))
				exp(p)(AS((x,p1,y,p2,w), ws))
			case AS(x, cons(p1, xs))
					if (sts(x,p1) < 0 && p < parens(p1) && connectf) =>
				val AS(y, ys)  = exp(0)(AS(null, xs))
				val AS(p2, zs) = eat(endParens(p1))(AS(null, ys))
				exp(p)(AS((x,p1,y,p2), zs))
			case AS(x, cons(op, xs)) if (p <= postfixs(op) && postfixs(op) == 0) =>
				val AS(y, ys) = exp(0)(AS(null, xs))
				if(y==null)AS((x,op),xs)
				else exp(0)(AS(((x,op), "@", y), ys))
			case AS(x, cons(op, xs)) if (p <= postfixs(op)) =>
				AS((x,op),xs)
			case AS(x, cons(op, xs)) if (p < infixs(op)) =>
				val AS(y, ys) = exp(infixs(op))(AS(null, xs))
				exp(p)(AS((x, op, y), ys))
			case AS(x, cons(op, xs)) if (p <=infixrs(op))=>
				val AS(y, ys) = exp(infixrs(op))(AS(null, xs))
				exp(p)(AS((x, op, y), ys))
			case AS(_, cons(x,xs)) if(eox(x)>0) => as
			case AS(null, xs) => as
			case AS(x, xs) if(xs == Nil) => as
			case AS(x, xs) if (p <= 0) =>
				val AS(y, ys) = exp(0)(AS(null, xs))
				if(y != Nil) exp(0)(AS((x, "@", y), ys))
				else                 AS(x, xs)
			case as => as
			}
		}
		exp(0)(AS(null, tokens)).a
	}
}
