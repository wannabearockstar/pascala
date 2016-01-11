package lexical.builders.keywords

import lexical.tokens.keywords.PrimaryType.INTEGER
import lexical.tokens.keywords.Reserved.PROCEDURE
import lexical.tokens.keywords.{IdentifierToken, ReservedToken, TypeToken}
import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class ReservedTokenBuilderSpec extends FlatSpec with Matchers {

	"A reserved token builder" should "accept letters or underscore" in {
		val builder = new PrimaryTypeTokenBuilder

		builder.isAccept('a') should be(true)
		builder.isAccept('_') should be(true)
		builder.isAccept('2') should be(false)
		builder.isAccept('$') should be(false)
		builder.isAccept('*') should be(false)
	}

	it should "accept case-insensitive strings" in {
		val builder = new ReservedTokenBuilder

		"PrOcEdURe" foreach builder.append
		val reservedToken = builder.build(0, 0)

		reservedToken shouldBe a[ReservedToken]
		reservedToken.value should be(PROCEDURE)
	}

	it should "build primary type token if string is not reserved word and primary type" in {
		val builder = new ReservedTokenBuilder

		"Integer" foreach builder.append
		val intToken = builder.build(0, 0)

		intToken shouldBe a[TypeToken]
		intToken.value should be(INTEGER)
	}

	it should "build identifier token if string not reserved word and not primary type" in {
		val builder = new ReservedTokenBuilder

		"some_word" foreach builder.append
		val intToken = builder.build(0, 0)

		intToken shouldBe a[IdentifierToken]
		intToken.value should be("some_word")
	}
}
