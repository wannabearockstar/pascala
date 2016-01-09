package lexical.tokens.keywords

/**
	* Created by wannabe on 02.01.16.
	*/
object Reserved {

	def fromString(s: String): Option[Reserved] = s.toLowerCase match {
		case "begin" => Some(BEGIN)
		case "case" => Some(CASE)
		case "const" => Some(CONST)
		case "do" => Some(DO)
		case "else" => Some(ELSE)
		case "end" => Some(END)
		case "for" => Some(FOR)
		case "function" => Some(FUNCTION)
		case "goto" => Some(GOTO)
		case "if" => Some(IF)
		case "procedure" => Some(PROCEDURE)
		case "program" => Some(PROGRAM)
		case "then" => Some(THEN)
		case "type" => Some(TYPE)
		case "while" => Some(WHILE)
		case "with" => Some(WITH)
		case "var" => Some(VAR)
		case _ => None
	}

	sealed trait Reserved

	case object BEGIN extends Reserved

	case object CASE extends Reserved

	case object CONST extends Reserved

	case object DO extends Reserved

	case object ELSE extends Reserved

	case object END extends Reserved

	case object FOR extends Reserved

	case object FUNCTION extends Reserved

	case object GOTO extends Reserved

	case object IF extends Reserved

	case object PROCEDURE extends Reserved

	case object PROGRAM extends Reserved

	case object THEN extends Reserved

	case object TYPE extends Reserved

	case object WHILE extends Reserved

	case object WITH extends Reserved

	case object VAR extends Reserved

}
