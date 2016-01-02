package lexical.tokens.keywords

/**
	* Created by wannabe on 02.01.16.
	*/
object Separate {

	def fromString(s: String): Option[Separate] = s match {
		case "{" => Some(LEFT_BRACE)
		case "}" => Some(RIGHT_BRACE)
		case "[" => Some(LEFT_BRACKET)
		case "]" => Some(RIGHT_BRACKET)
		case "(" => Some(LEFT_PARENTHESIS)
		case ")" => Some(RIGHT_PARENTHESIS)
		case ":" => Some(COLON)
		case ";" => Some(SEMICOLON)
		case "," => Some(COMMA)
		case "." => Some(DOT)
		case _ => None
	}

	sealed trait Separate

	case object LEFT_BRACE extends Separate

	case object RIGHT_BRACE extends Separate

	case object LEFT_BRACKET extends Separate

	case object RIGHT_BRACKET extends Separate

	case object LEFT_PARENTHESIS extends Separate

	case object RIGHT_PARENTHESIS extends Separate

	case object COLON extends Separate

	case object SEMICOLON extends Separate

	case object COMMA extends Separate

	case object DOT extends Separate

}
