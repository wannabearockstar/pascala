package lexical.tokens.const

import java.util.function.Predicate

import lexical.tokens.Token

/**
	* Created by wannabe on 29.12.15.
	*/
class IntToken(override val line: Int, override val position: Int, override val text: String) extends Token[Int](line, position, text) {
	override val value = text.toInt
}
