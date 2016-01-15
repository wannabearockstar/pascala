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

		tree.nonTerminal should be(Expression)
		tree.value.isDefined should be(false)

		tree.children.head.value.get should be(an[IdentifierToken])
		tree.children.head.value.get.value should be("x")

		tree.children(1).value.get should be(an[OperatorToken])
		tree.children(1).value.get.value should be(EQUALS)

		tree.children(2).nonTerminal should be(SimpleExpression)
		tree.children(2).value.isDefined should be(false)

		tree.children(2).children.head.value.get should be(an[IntToken])
		tree.children(2).children.head.value.get.value should be(1)

		tree.children(2).children(1).value.get should be(an[OperatorToken])
		tree.children(2).children(1).value.get.value should be(PLUS)

		tree.children(2).children(2).value.get should be(an[IntToken])
		tree.children(2).children(2).value.get.value should be(2)
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

		tree.value.isDefined should be(false)
		tree.nonTerminal should be(Expression)

		tree.children.head.value.get should be(an[IdentifierToken])
		tree.children.head.value.get.value should be("x")

		tree.children(1).value.get should be(an[OperatorToken])
		tree.children(1).value.get.value should be(EQUALS)

		tree.children(2).nonTerminal should be(Expression)
		tree.children(2).value.isDefined should be(false)

		tree.children(2).children.head.value.get should be(an[IntToken])
		tree.children(2).children.head.value.get.value should be(1)

		tree.children(2).children(1).value.get should be(an[OperatorToken])
		tree.children(2).children(1).value.get.value should be(GREATER)

		tree.children(2).children(2).value.get should be(an[IntToken])
		tree.children(2).children(2).value.get.value should be(2)
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

		tree.value.isDefined should be(false)
		tree.nonTerminal should be(Expression)

		tree.children.head.value.get should be(an[IdentifierToken])
		tree.children.head.value.get.value should be("x")

		tree.children(1).value.get should be(an[OperatorToken])
		tree.children(1).value.get.value should be(EQUALS)

		tree.children(2).nonTerminal should be(SimpleExpression)
		tree.children(2).value.isDefined should be(false)

		tree.children(2).children.head.value.get should be(an[IntToken])
		tree.children(2).children.head.value.get.value should be(1)

		tree.children(2).children(1).value.get should be(an[OperatorToken])
		tree.children(2).children(1).value.get.value should be(PLUS)

		tree.children(2).children(2).nonTerminal should be(SimpleExpression)
		tree.children(2).children(2).value.isDefined should be(false)

		tree.children(2).children(2).children.head.nonTerminal should be(Term)
		tree.children(2).children(2).children.head.value.isDefined should be(false)

		tree.children(2).children(2).children.head.children.head.value.get should be(an[IntToken])
		tree.children(2).children(2).children.head.children.head.value.get.value should be(2)

		tree.children(2).children(2).children.head.children(1).value.get should be(an[OperatorToken])
		tree.children(2).children(2).children.head.children(1).value.get.value should be(MULTIPLY)

		tree.children(2).children(2).children.head.children(2).value.get should be(an[IntToken])
		tree.children(2).children(2).children.head.children(2).value.get.value should be(3)

		tree.children(2).children(2).children(1).value.get should be(an[OperatorToken])
		tree.children(2).children(2).children(1).value.get.value should be(MINUS)

		tree.children(2).children(2).children(2).value.get should be(an[IntToken])
		tree.children(2).children(2).children(2).value.get.value should be(4)
	}
}
