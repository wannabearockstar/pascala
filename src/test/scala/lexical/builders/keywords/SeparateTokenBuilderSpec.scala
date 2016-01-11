package lexical.builders.keywords

import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class SeparateTokenBuilderSpec extends FlatSpec with Matchers {

	"A separate token builder" should "accept only separate symbols" in {
		val builder = new SeparateTokenBuilder

		builder.isAccept(',') should be(true)
		builder.isAccept('.') should be(true)
		builder.isAccept(':') should be(true)
		builder.isAccept(';') should be(true)
		builder.isAccept('1') should be(false)
		builder.isAccept('a') should be(false)
		builder.isAccept('-') should be(false)
	}

	it should "accept only one-length strings" in {
		val builder = new SeparateTokenBuilder

		builder.isAccept(',') should be(true)
		builder.append(',')
		builder.isAccept(',') should be(false)
	}
}
