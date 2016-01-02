package lexical.tokens.keywords

import lexical.tokens.keywords.PrimaryType.PrimaryType
import lexical.tokens.Token

/**
	* Created by wannabe on 31.12.15.
	*/
class TypeToken(override val line: Int, override val position: Int, override val text: String)
	extends Token[PrimaryType](line, position, text) {
	override val value = PrimaryType.fromString(text).get
}
