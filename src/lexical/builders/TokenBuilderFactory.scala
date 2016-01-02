package lexical.builders

import lexical.builders.cond.OperatorTokenBuilder
import lexical.builders.const.{DoubleTokenBuilder, IntTokenBuilder, StringTokenBuilder}
import lexical.builders.keywords.{ReservedTokenBuilder, SeparateTokenBuilder, IdentifierTokenBuilder, PrimaryTypeTokenBuilder}

/**
	* Created by wannabe on 02.01.16.
	*/
object TokenBuilderFactory {

	def apply(kind: String): TokenBuilder = kind match {
		case "operator" => new OperatorTokenBuilder
		case "const:double" => new DoubleTokenBuilder
		case "const:int" => new IntTokenBuilder
		case "const:string" => new StringTokenBuilder
		case "primary_type" => new PrimaryTypeTokenBuilder
		case "identifier" => new IdentifierTokenBuilder
		case "separator" => new SeparateTokenBuilder
		case "reserved" => new ReservedTokenBuilder
		case _ => throw new IllegalArgumentException()
	}
}
