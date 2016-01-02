package lexical.tokens.keywords

import lexical.tokens.Token

/**
	* Created by wannabe on 02.01.16.
	*/
class IdentifierToken(override val line: Int, override val position: Int, override val text: String) extends Token[String](line, position, text) {
	override val value = text
}
