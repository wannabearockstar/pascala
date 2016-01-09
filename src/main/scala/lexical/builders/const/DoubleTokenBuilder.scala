package lexical.builders.const

import lexical.builders.TokenBuilder
import lexical.tokens.Token
import lexical.tokens.const.{IntToken, DoubleToken}

/**
	* Created by wannabe on 02.01.16.
	*/
class DoubleTokenBuilder extends TokenBuilder {

	val pattern = "(\\d+\\.\\d+)".r

	override def isAccept(c: Char): Boolean = c.isDigit || (builder.nonEmpty && c == '.')

	override def build(line: Int, position: Int): Token[_] = builder.mkString match {
		case pattern(c) => new DoubleToken(line, position, c)
		case intString => new IntToken(line, position, intString)
	}
}
