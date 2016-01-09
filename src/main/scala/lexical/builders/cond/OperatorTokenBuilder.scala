package lexical.builders.cond

import lexical.builders.TokenBuilder
import lexical.tokens.Token
import lexical.tokens.cond.{Operator, OperatorToken}

/**
	* Created by wannabe on 02.01.16.
	*/
class OperatorTokenBuilder extends TokenBuilder{

	override def build(line: Int, position: Int): Token[_] = new OperatorToken(line, position, builder.mkString)

	override def isValidNextCharacter(c: Char): Boolean = builder.isEmpty && Operator.fromString(c.toString).isDefined
}
