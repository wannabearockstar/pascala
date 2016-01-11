package parser

import lexical.tokens.cond.Operator._
import lexical.tokens.cond.OperatorToken
import lexical.tokens.const.IntToken
import lexical.tokens.keywords.IdentifierToken
import org.scalatest.{FlatSpec, Matchers}

/**
	* Created by wannabe on 11.01.16.
	*/
class Parser$Spec extends FlatSpec with Matchers {

	"A Parser" should "parse simple arithmetic expressions" in {
		val arithmeticTokens = List(
			Some(new IdentifierToken(0, 0, "x")),
			Some(new OperatorToken(1, 0, "=")),
			Some(new IntToken(0, 0, "1")),
			Some(new OperatorToken(1, 0, "+")),
			Some(new IntToken(0, 2, "2"))
		).iterator.buffered
		val tree = Parser.parse(arithmeticTokens)
		println(tree.asHorizontalString())

		tree.l.get.v.get should be(an[IdentifierToken])
		tree.l.get.v.get.value should be("x")
		tree.l.get.r should be(None)
		tree.l.get.l should be(None)

		tree.v.get should be(an[OperatorToken])
		tree.v.get.value should be(EQUALS)

		tree.r.get.v.get should be(an[OperatorToken])
		tree.r.get.v.get.value should be(PLUS)

		tree.r.get.l.get.v.get should be(an[IntToken])
		tree.r.get.l.get.v.get.value should be(1)
		tree.r.get.l.get.r should be(None)
		tree.r.get.l.get.l should be(None)

		tree.r.get.r.get.v.get should be(an[IntToken])
		tree.r.get.r.get.v.get.value should be(2)
		tree.r.get.r.get.r should be(None)
		tree.r.get.r.get.l should be(None)
	}

	it should "parse nested expressions" in {
		val arithmeticTokens = List(
			Some(new IdentifierToken(0, 0, "x")),
			Some(new OperatorToken(1, 0, "=")),
			Some(new IntToken(0, 0, "1")),
			Some(new OperatorToken(1, 0, ">")),
			Some(new IntToken(0, 2, "2"))
		).iterator.buffered
		val tree = Parser.parse(arithmeticTokens)

		tree.l.get.v.get should be(an[IdentifierToken])
		tree.l.get.v.get.value should be("x")
		tree.l.get.r should be(None)
		tree.l.get.l should be(None)

		tree.v.get should be(an[OperatorToken])
		tree.v.get.value should be(EQUALS)

		tree.r.get.v.get should be(an[OperatorToken])
		tree.r.get.v.get.value should be(GREATER)

		tree.r.get.l.get.v.get should be(an[IntToken])
		tree.r.get.l.get.v.get.value should be(1)
		tree.r.get.l.get.r should be(None)
		tree.r.get.l.get.l should be(None)

		tree.r.get.r.get.v.get should be(an[IntToken])
		tree.r.get.r.get.v.get.value should be(2)
		tree.r.get.r.get.r should be(None)
		tree.r.get.r.get.l should be(None)
	}

	it should "parse arithmetic operation with multiply priority" in {
		val arithmeticTokens = List(
			Some(new IdentifierToken(0, 0, "x")),
			Some(new OperatorToken(1, 0, "=")),
			Some(new IntToken(0, 0, "1")),
			Some(new OperatorToken(1, 0, "+")),
			Some(new IntToken(0, 2, "2")),
			Some(new OperatorToken(1, 0, "*")),
			Some(new IntToken(0, 2, "3")),
			Some(new OperatorToken(1, 0, "-")),
			Some(new IntToken(0, 2, "4"))
		).iterator.buffered
		val tree = Parser.parse(arithmeticTokens)

		tree.l.get.v.get should be(an[IdentifierToken])
		tree.l.get.v.get.value should be("x")
		tree.l.get.r should be(None)
		tree.l.get.l should be(None)

		tree.v.get should be(an[OperatorToken])
		tree.v.get.value should be(EQUALS)

		tree.r.get.v.get should be(an[OperatorToken])
		tree.r.get.v.get.value should be(PLUS)

		tree.r.get.l.get.v.get should be(an[IntToken])
		tree.r.get.l.get.v.get.value should be(1)
		tree.r.get.l.get.r should be(None)
		tree.r.get.l.get.l should be(None)

		tree.r.get.r.get.v.get should be(an[OperatorToken])
		tree.r.get.r.get.v.get.value should be(MINUS)

		tree.r.get.r.get.l.get.v.get should be(an[OperatorToken])
		tree.r.get.r.get.l.get.v.get.value should be(MULTIPLY)

		tree.r.get.r.get.l.get.l.get.v.get should be(an[IntToken])
		tree.r.get.r.get.l.get.l.get.v.get.value should be(2)

		tree.r.get.r.get.l.get.r.get.v.get should be(an[IntToken])
		tree.r.get.r.get.l.get.r.get.v.get.value should be(3)

		tree.r.get.r.get.r.get.v.get should be(an[IntToken])
		tree.r.get.r.get.r.get.v.get.value should be(4)
	}
}
