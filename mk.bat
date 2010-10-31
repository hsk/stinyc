call scalac ast.scala
yacc -J -Jpackage=stinyc -Jextends=stinyc.Ast  cparser.y
copy CLex.java stinyc\.
copy Parser.java stinyc\.
copy ParserVal.java stinyc\.
javac -classpath "C:\Program Files\Scala\lib\scala-library.jar;." stinyc/CLex.java stinyc/Parser.java stinyc/ParserVal.java
call scalac interp.scala
call scalac x86_code_gen.scala

