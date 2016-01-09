import lexical.Lexer
import parser.Parser

/**
	* Created by wannabe on 29.12.15.
	*/
object Main {
	def main (args: Array[String]) {
		val lexer: Lexer = new Lexer(scala.io.Source.fromFile("file.txt").buffered)
		print(Parser.parse(lexer).asHorizontalString())
	}
}
