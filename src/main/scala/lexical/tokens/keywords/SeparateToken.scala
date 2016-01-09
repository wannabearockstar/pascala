package lexical.tokens.keywords

import lexical.tokens.Token
import lexical.tokens.keywords.Separate.Separate

/**
	* Created by wannabe on 02.01.16.
	*/
class SeparateToken(override val line: Int, override val position: Int, override val text: String)
	extends Token[Separate](line, position, text){
	override val value = Separate.fromString(text).get
}
