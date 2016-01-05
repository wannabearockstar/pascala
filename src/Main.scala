import lexical.Lexer

/**
	* Created by wannabe on 29.12.15.
	*/
object Main {
	def main (args: Array[String]) {
		val lexer: Lexer = new Lexer(scala.io.Source.fromFile("file.txt").buffered)
		lexer foreach(i => println(s"${i.get.value} ${i.get.getClass.toString} ${i.get.line}_${i.get.position}"))
	}
}
