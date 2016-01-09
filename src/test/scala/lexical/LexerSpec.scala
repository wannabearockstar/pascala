package lexical

import lexical.tokens.cond.Operator.{Operator, EQUALS}
import lexical.tokens.const.IntToken
import lexical.tokens.keywords.PrimaryType.{INTEGER, PrimaryType}
import lexical.tokens.keywords.Reserved._
import lexical.tokens.keywords.Separate._
import lexical.tokens.keywords._
import org.scalatest._

/**
	* Created by wannabe on 09.01.16.
	*/
class LexerSpec extends FlatSpec with Matchers {

	"A Lexer" should "identify basic tokens" in {
		val basicTokens = "Program test; var a: Integer; begin a := 10; writeln(a); do end."
		val lexer = new Lexer(basicTokens.iterator.buffered)

		lexer.next().get.value should (be (an[Reserved]) and equal (PROGRAM))
		lexer.next().get shouldBe a [IdentifierToken]
		lexer.next().get.value should (be (an[Separate]) and equal (SEMICOLON))

		lexer.next().get.value should (be (an[Reserved]) and equal (VAR))
		lexer.next().get shouldBe a [IdentifierToken]
		lexer.next().get.value should (be (an[Separate]) and equal (COLON))
		lexer.next().get.value should (be (an[PrimaryType]) and equal (INTEGER))
		lexer.next().get.value should (be (an[Separate]) and equal (SEMICOLON))

		lexer.next().get.value should (be (an[Reserved]) and equal (BEGIN))
		lexer.next().get shouldBe a [IdentifierToken]
		lexer.next().get.value should (be (an[Separate]) and equal (COLON))
		lexer.next().get.value should (be (an[Operator]) and equal (EQUALS))
		lexer.next().get.value should equal (10)
		lexer.next().get.value should (be (an[Separate]) and equal (SEMICOLON))

		lexer.next().get shouldBe a [IdentifierToken]
		lexer.next().get.value should (be (an[Separate]) and equal (LEFT_PARENTHESIS))
		lexer.next().get shouldBe a [IdentifierToken]
		lexer.next().get.value should (be (an[Separate]) and equal (RIGHT_PARENTHESIS))
		lexer.next().get.value should (be (an[Separate]) and equal (SEMICOLON))

		lexer.next().get.value should (be (an[Reserved]) and equal (DO))
		lexer.next().get.value should (be (an[Reserved]) and equal (END))
		lexer.next().get.value should (be (an[Separate]) and equal (DOT))
	}
}
