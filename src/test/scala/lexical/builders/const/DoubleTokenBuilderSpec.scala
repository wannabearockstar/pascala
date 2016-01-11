package lexical.builders.const

import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class DoubleTokenBuilderSpec extends FlatSpec with Matchers {

	"A Double token builder" should "accept only digits when empty" in {
		val builder = new DoubleTokenBuilder

		all(0 until 9 map (x => builder.isAccept(x.toString.charAt(0)))) should be(true)
		builder.isAccept('a') should be(false)
		builder.isAccept('.') should be(false)
	}

	it should "accept dot as well when non-empty" in {
		val builder = new DoubleTokenBuilder

		builder.isAccept('.') should be(false)
		builder.append('1')
		builder.isAccept('.') should be(true)
	}
}
