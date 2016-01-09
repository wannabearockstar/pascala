package parser

import lexical.tokens.Token
import lexical.tokens.cond.Operator._
import lexical.tokens.cond.OperatorToken
import parser.utils.Tree

/**
	* Created by wannabe on 05.01.16.
	*/
sealed trait NonTerminal {

	def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]]
}

case object Expression extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(None, None, None)
		tree.l = Some(SimpleExpression.evaluate(tokens))
		if (tokens.hasNext) {
			return tokens.head.get match {
				case operator: OperatorToken => operator.value match {
					case EQUALS | GREATER | LESS =>
						tree.v = tokens.next()
						tree.r = Some(Expression.evaluate(tokens))
						tree
					case _ => tree.l.get
				}
				case _ => tree.l.get
			}
		}
		tree.l.get
	}
}

case object SimpleExpression extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(None, None, None)
		tree.l = Some(Term.evaluate(tokens))
		if (tokens.hasNext) {
			return tokens.head.get match {
				case operator: OperatorToken => operator.value match {
					case PLUS | MINUS =>
						tree.v = tokens.next()
						tree.r = Some(SimpleExpression.evaluate(tokens))
						tree
					case _ => tree.l.get
				}
			}
		}
		tree.l.get
	}
}

case object Term extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(None, None, None)
		if (tokens.hasNext) {
			tree.l = Some(new Tree(tokens.next(), None, None))
			if (tokens.hasNext) {
				return tokens.head.get match {
					case operator: OperatorToken => operator.value match {
						case MULTIPLY | DIVIDE =>
							tree.v = tokens.next()
							tree.r = Some(Term.evaluate(tokens))
							tree
						case _ => tree.l.get
					}
				}
			}
		}
		tree.l.get
	}
}

object Parser {

	def parse(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		Expression.evaluate(tokens)
	}
}
