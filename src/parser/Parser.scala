package parser

import lexical.tokens.Token
import lexical.tokens.cond.OperatorToken
import lexical.tokens.const.{StringToken, IntToken, DoubleToken}
import lexical.tokens.keywords.Separate.{RIGHT_PARENTHESIS, LEFT_PARENTHESIS}
import lexical.tokens.keywords.{IdentifierToken, SeparateToken}
import parser.rules.{OperatorRule, OperandRule, RightParenthesisRule, LeftParenthesisRule}
import parser.utils.Tree

import scala.collection.mutable
import scala.collection.mutable.Stack

/**
	* Created by wannabe on 05.01.16.
	*/
object Parser {

	def resolveHandler(token: Option[Token[_]]) = token.get match {
		case separate: SeparateToken => separate.value match {
			case LEFT_PARENTHESIS => new LeftParenthesisRule
			case RIGHT_PARENTHESIS => new RightParenthesisRule
			case _ => throw new IllegalArgumentException
		}
		case operator: OperatorToken => new OperatorRule
		case operand: IdentifierToken => new OperandRule
		case const: DoubleToken => new OperandRule
		case const: IntToken => new OperandRule
		case const: StringToken => new OperandRule
		case _ => throw new IllegalArgumentException

	}

	def parse(tokens: Iterator[Option[Token[_]]]): Tree[Token[_]] = {
		var tree: Tree[Token[_]] = new Tree(None, None, None)
		val head = tree
		val stack = new mutable.Stack[Tree[Token[_]]]
		tokens foreach {
			token => tree = resolveHandler(token).apply(tree, token, stack)
		}
		head
	}
}
