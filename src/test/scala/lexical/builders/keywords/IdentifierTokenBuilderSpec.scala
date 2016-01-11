package lexical.builders.keywords

import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class IdentifierTokenBuilderSpec extends FlatSpec with Matchers {

	"A identifier token builder" should "accept letter, digit or underscore" in {
		val builder = new IdentifierTokenBuilder

		builder.isAccept('d') should be(true)
		builder.isAccept('2') should be(true)
		builder.isAccept('_') should be(true)
		builder.isAccept('-') should be(false)
		builder.isAccept('.') should be(false)
		builder.isAccept('%') should be(false)
		builder.isAccept('$') should be(false)
		builder.isAccept('+') should be(false)
	}
}
