package lexical.builders.keywords

import lexical.builders.TokenBuilder
import lexical.tokens.Token
import lexical.tokens.keywords.{SeparateToken, Separate}

/**
	* Created by wannabe on 02.01.16.
	*/
class SeparateTokenBuilder extends TokenBuilder {

	override def build(line: Int, position: Int): Token[_] = Separate.fromString(builder.mkString) match {
		case Some(token) => new SeparateToken(line, position, builder.mkString)
	}

	override def isAccept(c: Char): Boolean = builder.isEmpty && Separate.fromString(c.toString).isDefined
}
