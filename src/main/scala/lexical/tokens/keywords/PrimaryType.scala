package lexical.tokens.keywords

/**
	* Created by wannabe on 31.12.15.
	*/
object PrimaryType {

	def fromString(s: String): Option[PrimaryType] = s match {
		case "Real" => Some(REAL)
		case "Integer" => Some(INTEGER)
		case "Char" => Some(CHAR)
		case "Boolean" => Some(BOOLEAN)
		case "String" => Some(STRING)
		case _ => None
	}

	sealed trait PrimaryType

	case object REAL extends PrimaryType

	case object INTEGER extends PrimaryType

	case object CHAR extends PrimaryType

	case object BOOLEAN extends PrimaryType

	case object STRING extends PrimaryType

}

