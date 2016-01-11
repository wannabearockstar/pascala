package lexical.builders.keywords

import lexical.tokens.keywords.PrimaryType.INTEGER
import lexical.tokens.keywords.{IdentifierToken, TypeToken}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
	* Created by wannabe on 11.01.16.
	*/
class PrimaryTypeTokenBuilderSpec extends FlatSpec with Matchers {

	"A primary type token builder" should "accept only letters and underscore" in {
		val builder = new PrimaryTypeTokenBuilder

		builder.isAccept('a') should be(true)
		builder.isAccept('_') should be(true)
		builder.isAccept('2') should be(false)
		builder.isAccept('$') should be(false)
		builder.isAccept('*') should be(false)
	}

	it should "build identifier token, if input string do not equals to any type" in {
		val builder = new PrimaryTypeTokenBuilder

		"Integer" foreach builder.append
		val intToken = builder.build(0, 0)
		builder.flush()

		intToken shouldBe a[TypeToken]
		intToken.value should be(INTEGER)

		"asd" foreach builder.append
		val identToken = builder.build(0, 0)

		identToken shouldBe a[IdentifierToken]
		identToken.value should be("asd")
	}
}
