package lexical.builders.keywords

import lexical.builders.TokenBuilder
import lexical.tokens.Token
import lexical.tokens.keywords._

/**
	* Created by wannabe on 02.01.16.
	*/
class ReservedTokenBuilder extends TokenBuilder {

	override def build(line: Int, position: Int): Token[_] = Reserved.fromString(builder.mkString) match {
		case Some(token) => new ReservedToken(line, position, builder.mkString)
		case None => PrimaryType.fromString(builder.mkString) match {
			case Some(token) => new TypeToken(line, position, builder.mkString)
			case None => new IdentifierToken(line, position, builder.mkString)
		}
	}

	override def isAccept(c: Char): Boolean = c.isLetter || (c equals '_')
}
