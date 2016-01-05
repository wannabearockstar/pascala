package lexical

import lexical.builders.cond.OperatorTokenBuilder
import lexical.builders.const.{DoubleTokenBuilder, IntTokenBuilder, StringTokenBuilder}
import lexical.builders.keywords.{ReservedTokenBuilder, SeparateTokenBuilder, IdentifierTokenBuilder, PrimaryTypeTokenBuilder}
import lexical.tokens.Token

import scala.language.existentials

/**
	* Created by wannabe on 29.12.15.
	*/
class Lexer(sourceIterator: BufferedIterator[Char]) extends Iterator[Option[Token[_]]] {

	var cachedNext: Option[Token[_]] = None
	val reader: Reader = new Reader(sourceIterator)
	var currentToken = null
	val rules = List(
		new ReservedTokenBuilder,
		new PrimaryTypeTokenBuilder,
		new OperatorTokenBuilder,
		new DoubleTokenBuilder,
		new IntTokenBuilder,
		new StringTokenBuilder,
		new IdentifierTokenBuilder,
		new SeparateTokenBuilder
	)

	def skipComments(): Boolean = {
		if (reader.lastChar == '/' && reader.buf.head == '/') {
			while (reader.hasNext && reader.next() != '\n') {}
			return true
		}
		false
	}

	override def next(): Option[Token[_]] = {
		if (cachedNext.isDefined) {
			val extractedNext = cachedNext
			cachedNext = None
			return extractedNext
		}
		skipWhitespacesAndNewlines()
		reader.hasNext match {
			case false => None
			case true =>
				val tokenBuilder = rules.toStream.find(_.isValidNextCharacter(reader.buf.head)).get
					.getClass.newInstance()
				while (reader.hasNext && tokenBuilder.isValidNextCharacter(reader.buf.head)) {
					tokenBuilder.append(reader.next())
					if (skipComments()) {
						return next()
					}
				}
				Some(tokenBuilder.build(reader.line, reader.position - tokenBuilder.builder.length))
		}
	}

	def skipWhitespacesAndNewlines() = {
		while (reader.hasNext && (reader.buf.head.isWhitespace || (reader.buf.head equals '\n'))) reader.next()
	}

	override def hasNext: Boolean = {
		if (cachedNext.isDefined) {
			return true
		}
		cachedNext = next()
		cachedNext.isDefined
	}
}

class Reader(val buf: BufferedIterator[Char]) {

	var line = 0
	var position = 0
	var lastChar = buf.head

	def next(): Char = {
		buf.head match {
			case '\n' => line += 1
			case char => position += 1
		}
		lastChar = buf.next()
		lastChar
	}

	def hasNext: Boolean = buf.hasNext
}
