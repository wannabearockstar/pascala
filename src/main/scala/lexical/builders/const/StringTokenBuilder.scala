package lexical.builders.const

import lexical.builders.TokenBuilder
import lexical.tokens.Token
import lexical.tokens.const.StringToken

/**
	* Created by wannabe on 02.01.16.
	*/
class StringTokenBuilder extends TokenBuilder {

	override def build(line: Int, position: Int): Token[_] = new StringToken(line, position, builder.mkString)

	override def isValidNextCharacter(c: Char): Boolean = {
		builder.isEmpty match {
			case true => c equals '\''
			case false => builder.length match {
				case 1 => true
				case _ => !(builder.last equals '\'')
			}
		}
	}
}
