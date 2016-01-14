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

//case object Program extends NonTerminal {
//
//	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
//		val tree: Tree[Token[_]] = new Tree(None, None, None)
//		//todo check if program starts with PROGRAM %program_name%; begin %% end
////		tokens.next()
////		tokens.next()
////		tokens.next()
////		tokens.next()
//		//todo
//	}
//}

case object Expression extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(Expression)
		val operand = SimpleExpression.evaluate(tokens)
		tree.children = tree.children :+ operand
		if (tokens.hasNext) {
			return tokens.head.get match {
				case operator: OperatorToken => operator.value match {
					case EQUALS | GREATER | LESS =>
						tree.children = tree.children :+ new Tree[Token[_]](Expression, List.empty, tokens.next())
						tree.children = tree.children :+ SimpleExpression.evaluate(tokens)
						tree
					case _ => operand
				}
				case _ => operand
			}
		}
		operand
	}
}

case object SimpleExpression extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(SimpleExpression)
		val operand = Term.evaluate(tokens)
		tree.children = tree.children :+ operand
		if (tokens.hasNext) {
			return tokens.head.get match {
				case operator: OperatorToken => operator.value match {
					case PLUS | MINUS =>
						tree.children = tree.children :+ new Tree[Token[_]](SimpleExpression, List.empty, tokens.next())
						tree.children = tree.children :+ SimpleExpression.evaluate(tokens)
						tree
					case _ => operand
				}
			}
		}
		operand
	}
}

case object Term extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(Term)
		val operand = new Tree(Term, List.empty, tokens.next())
		tree.children = tree.children :+ operand
			if (tokens.hasNext) {
				return tokens.head.get match {
					case operator: OperatorToken => operator.value match {
						case MULTIPLY | DIVIDE =>
							tree.children = tree.children :+ new Tree[Token[_]](Term, List.empty, tokens.next())
							tree.children = tree.children :+ Term.evaluate(tokens)
							tree
						case _ => operand
					}
				}
			}
		operand
	}
}

object Parser {

	def parse(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		Expression.evaluate(tokens)
	}
}
