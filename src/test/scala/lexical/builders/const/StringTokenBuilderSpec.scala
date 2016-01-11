package lexical.builders.const

import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class StringTokenBuilderSpec extends FlatSpec with Matchers {

	"A string token builder" should "accept quote and then any symbol" in {
		val builder = new StringTokenBuilder

		builder.isAccept('s') should be(false)
		builder.isAccept('\'') should be(true)

		builder.append('\'')
		builder.isAccept('s') should be(true)
	}

	it should "not accept any symbol after closing quote" in {
		val builder = new StringTokenBuilder

		builder.append('\'')
		builder.isAccept('s') should be(true)

		builder.append('s')
		builder.append('\'')
		builder.isAccept('s') should be(false)
		builder.isAccept('\'') should be(false)
	}
}
