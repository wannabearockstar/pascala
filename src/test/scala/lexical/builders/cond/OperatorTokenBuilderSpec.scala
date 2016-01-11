package lexical.builders.cond

import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class OperatorTokenBuilderSpec extends FlatSpec with Matchers {

	"A Operator token builder" should "accept only operators" in {
		val builder = new OperatorTokenBuilder

		builder.isAccept('=') should be(true)
		builder.isAccept('-') should be(true)
		builder.isAccept('+') should be(true)
		builder.isAccept('/') should be(true)
		builder.isAccept('*') should be(true)

		builder.isAccept('s') should be(false)
		builder.isAccept('1') should be(false)
		builder.isAccept('.') should be(false)
		builder.isAccept('_') should be(false)
	}

	it should "accept only one-length operators" in {
		val builder = new OperatorTokenBuilder

		builder.isAccept('=') should be(true)
		builder.append('=')
		builder.isAccept('=') should be(false)
	}
}
