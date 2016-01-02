package lexical.tokens.keywords

import lexical.tokens.Token
import lexical.tokens.keywords.Reserved.Reserved

/**
	* Created by wannabe on 02.01.16.
	*/
class ReservedToken(override val line: Int, override val position: Int, override val text: String)
	extends Token[Reserved](line, position, text) {
	override val value = Reserved.fromString(text).get
}
