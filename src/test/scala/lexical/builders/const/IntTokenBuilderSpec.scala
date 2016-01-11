package lexical.builders.const

import lexical.tokens.cond.Operator.MINUS
import lexical.tokens.cond.OperatorToken
import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class IntTokenBuilderSpec extends FlatSpec with Matchers {

	"A Int token builder" should "accept only digits when non-empty" in {
		val builder = new IntTokenBuilder

		all(0 until 9 map (x => builder.isAccept(x.toString.charAt(0)))) should be(true)
	}

	it should "accept minus as first symbol" in {
		val builder = new IntTokenBuilder

		builder.isAccept('-') should be(true)
		builder.append('0')
		builder.isAccept('-') should be(false)
	}

	it should "build minus operator if contain only him" in {
		val builder = new IntTokenBuilder

		builder.append('-')
		builder.build(0, 0) shouldBe a[OperatorToken]
		builder.flush()

		builder.append('-')
		builder.build(0, 0).value should be(MINUS)
	}
}
