package lexical.tokens.const

import lexical.tokens.Token
import lexical.tokens.utils.Argument

/**
	* Created by wannabe on 29.12.15.
	*/
class IntToken(override val line: Int, override val position: Int, override val text: String) extends Token[Int](line, position, text) with Argument {
	override val value = text.toInt
}
