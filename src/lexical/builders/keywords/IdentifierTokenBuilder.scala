package lexical.builders.keywords

import lexical.builders.TokenBuilder
import lexical.tokens.Token
import lexical.tokens.keywords.IdentifierToken

/**
	* Created by wannabe on 02.01.16.
	*/
class IdentifierTokenBuilder extends TokenBuilder{

	override def build(line: Int, position: Int): Token[_] = new IdentifierToken(line, position, builder.mkString)

	override def isValidNextCharacter(c: Char): Boolean = c.isLetterOrDigit || (c equals '_')
}
