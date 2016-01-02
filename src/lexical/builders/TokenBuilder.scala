package lexical.builders

import lexical.tokens.Token

/**
	* Created by wannabe on 02.01.16.
	*/
abstract class TokenBuilder {

	val builder = new StringBuilder

	def build(line: Int, position: Int): Token[_]

	def isValidNextCharacter(c: Char): Boolean

	def append(c: Char) = isValidNextCharacter(c) match {
		case false => throw new IllegalArgumentException()
		case true => builder.append(c.toString)
	}
}
