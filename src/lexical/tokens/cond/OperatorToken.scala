package lexical.tokens.cond

import lexical.tokens.Token
import lexical.tokens.cond.Operator.Operator

/**
	* Created by wannabe on 29.12.15.
	*/
class OperatorToken(override val line: Int, override val position: Int, override val text: String)
	extends Token[Operator](line, position, text) {
		override val value = Operator.fromString(text).get
	}
