package lexical.builders.const

import lexical.builders.TokenBuilder
import lexical.tokens.Token
import lexical.tokens.cond.OperatorToken
import lexical.tokens.const.IntToken

/**
	* Created by wannabe on 02.01.16.
	*/
class IntTokenBuilder extends TokenBuilder {

	override def isAccept(c: Char): Boolean = builder.isEmpty match {
		case true => c.isDigit || (c equals '-')
		case false => c.isDigit
	}

	override def build(line: Int, position: Int): Token[_] = builder.mkString match {
		case "-" => new OperatorToken(line, position, "-")
		case intString => new IntToken(line, position, intString)
	}
}