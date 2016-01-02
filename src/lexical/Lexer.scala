package lexical

import lexical.builders.TokenBuilderFactory
import lexical.builders.cond.OperatorTokenBuilder
import lexical.builders.const.{DoubleTokenBuilder, IntTokenBuilder, StringTokenBuilder}
import lexical.builders.keywords.{ReservedTokenBuilder, SeparateTokenBuilder, IdentifierTokenBuilder, PrimaryTypeTokenBuilder}
import lexical.tokens.Token

import scala.language.existentials

/**
	* Created by wannabe on 29.12.15.
	*/
class Lexer(filename: String) {

	val reader: Reader = new Reader(filename)
	var currentToken = null
	val rules = List(
		(new ReservedTokenBuilder, "reserved"),
		(new PrimaryTypeTokenBuilder, "primary_type"),
		(new OperatorTokenBuilder, "operator"),
		(new DoubleTokenBuilder, "const:double"),
		(new IntTokenBuilder, "const:int"),
		(new StringTokenBuilder, "const:string"),
		(new IdentifierTokenBuilder, "identifier"),
		(new SeparateTokenBuilder, "separator")
	)

	def next(): Option[Token[_]] = {
		skipWhitespacesAndNewlines()
		reader.hasNext match {
			case false => None
			case true =>
				val tokenBuilder = TokenBuilderFactory(
					rules.toStream.find(_._1.isValidNextCharacter(reader.buf.head)).get._2
				)
				while (reader.hasNext && tokenBuilder.isValidNextCharacter(reader.buf.head)) {
					tokenBuilder.append(reader.next())
				}
				Some(tokenBuilder.build(reader.line, reader.position - tokenBuilder.builder.length))
		}
	}

	def skipWhitespacesAndNewlines() = {
		while (reader.hasNext && (reader.buf.head.isWhitespace || (reader.buf.head equals '\n'))) reader.next()
	}
}

class Reader(filename: String) {

	var line = 0
	var position = 0

	val buf = scala.io.Source.fromFile(filename).buffered

	def next(): Char = {
		buf.head match {
			case '\n' => line += 1
			case char => position += 1
		}
		buf.next()
	}

	def hasNext: Boolean = buf.hasNext
}
