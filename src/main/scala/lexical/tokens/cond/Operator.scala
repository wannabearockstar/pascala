package lexical.tokens.cond

/**
	* Created by wannabe on 29.12.15.
	*/
object Operator {

	def fromString(s: String): Option[Operator] = s match {
		case "+" => Some(PLUS)
		case "-" => Some(MINUS)
		case "/" => Some(DIVIDE)
		case "*" => Some(MULTIPLY)
		case "=" => Some(EQUALS)
		case ">" => Some(GREATER)
		case "<" => Some(LESS)
		case _ => None
	}

	sealed trait Operator

	case object PLUS extends Operator

	case object MINUS extends Operator

	case object DIVIDE extends Operator

	case object MULTIPLY extends Operator

	case object EQUALS extends Operator

	case object GREATER extends Operator

	case object LESS extends Operator

}
