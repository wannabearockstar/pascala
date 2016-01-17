package parser

import lexical.tokens.Token
import lexical.tokens.cond.Operator._
import lexical.tokens.cond.OperatorToken
import lexical.tokens.keywords.Reserved._
import lexical.tokens.keywords.Separate.{COLON, COMMA, SEMICOLON}
import lexical.tokens.keywords.{IdentifierToken, ReservedToken, SeparateToken, TypeToken}
import parser.utils.Tree

/**
	* Created by wannabe on 05.01.16.
	*/
sealed trait NonTerminal {

	def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]]
}

case object Program extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(Program)
		if (!tokens.next().get.value.equals(PROGRAM)) {
			throw new IllegalArgumentException
		}
		val programName = tokens.next().get.value.toString
		if (!tokens.next().get.value.equals(SEMICOLON)) {
			throw new IllegalArgumentException
		}

		while (tokens.hasNext) {
			tokens.next().get match {
				case reservedToken: ReservedToken => reservedToken.value match {
					case CONST => tree.children = tree.children :+ Const.evaluate(tokens)
					case VAR => tree.children = tree.children :+ Var.evaluate(tokens)
					case BEGIN => tree.children = tree.children :+ Expression.evaluate(tokens)
					case END => return tree
				}
			}
		}
		tree
	}
}

case object Const extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(Const)
		while (tokens.hasNext && tokens.head.get.isInstanceOf[IdentifierToken]) {
			tree.children = tree.children :+ Expression.evaluate(tokens)
		}
		tree
	}
}

case object Var extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(Var)
		while (tokens.hasNext && tokens.head.get.isInstanceOf[IdentifierToken]) {
			tree.children = tree.children :+ VarDeclaration.evaluate(tokens)
		}
		tree
	}
}

case object VarDeclaration extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(VarDeclaration)
		val operand = new Tree(Term, List.empty, tokens.next())
		if (tokens.hasNext) {
			return tokens.head.get match {
				case sepToken: SeparateToken => sepToken.value match {
					case COMMA =>
						tokens.next()
						tree.children = tree.children :+ operand
						tree.children = tree.children ::: VarDeclaration.evaluate(tokens).children
						tree
					case COLON =>
						tree.children = tree.children :+ operand
						tree.children = tree.children :+ new Tree(VarDeclaration, List.empty, tokens.next())
						tree.children = tree.children :+ VarDeclaration.evaluate(tokens)
						tree
					case SEMICOLON =>
						tokens.next()
						operand
					case _ => throw new IllegalArgumentException
				}
				case typeToken: TypeToken => new Tree(Term, List.empty, tokens.next())
				case _ => throw new IllegalArgumentException
			}
		}
		operand
	}
}

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
						tree.children = tree.children :+ Expression.evaluate(tokens)
						tree
					case _ => operand
				}
				case sepToken: SeparateToken => sepToken.value match {
					case SEMICOLON =>
						tokens.next()
						tree
					case _ => throw new IllegalArgumentException
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
				case _ => operand
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
				case _ => operand
			}
		}
		operand
	}
}

object Parser {

	def parse(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		Program.evaluate(tokens)
	}
}
