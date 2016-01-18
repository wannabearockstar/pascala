package parser

import lexical.tokens.Token
import lexical.tokens.cond.Operator._
import lexical.tokens.cond.OperatorToken
import lexical.tokens.const.{DoubleToken, IntToken, StringToken}
import lexical.tokens.keywords.Reserved._
import lexical.tokens.keywords.Separate._
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
		val programName = tokens.next()
		if (!tokens.next().get.value.equals(SEMICOLON)) {
			throw new IllegalArgumentException
		}
		tree.children = tree.children :+ new Tree[Token[_]](Term, List.empty, programName)
		tree.children = tree.children :+ ProgramBody.evaluate(tokens)
		tree
	}
}

case object ProgramBody extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(ProgramBody)
		while (tokens.hasNext) {
			tokens.next().get match {
				case reservedToken: ReservedToken => reservedToken.value match {
					case CONST => tree.children = tree.children :+ Const.evaluate(tokens)
					case VAR => tree.children = tree.children :+ Var.evaluate(tokens)
					case BEGIN => tree.children = tree.children :+ CompoundStatement.evaluate(tokens)
					case FUNCTION => tree.children = tree.children :+ Function.evaluate(tokens)
					case END =>
						tokens.next()
						return tree
				}
			}
		}
		tree
	}
}

case object FunctionCallOrIdentifier extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val identifier = tokens.next()
		val reloadIterator = (Iterator.single(identifier) ++ tokens).buffered
		tokens.head.get match {
			case sepToken: SeparateToken => sepToken.value match {
				case LEFT_PARENTHESIS => FunctionCall.evaluate(reloadIterator)
				case _ => new Tree(Term, List.empty, identifier)
			}
			case _ => new Tree(Term, List.empty, identifier)
		}
	}
}

case object FunctionCall extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(FunctionCall)
		tree.children = tree.children :+ new Tree(Term, List.empty, tokens.next()) //func name

		if (!tokens.next().get.value.equals(LEFT_PARENTHESIS)) {
			throw new IllegalArgumentException
		}

		while (tokens.hasNext && !tokens.head.get.value.equals(RIGHT_PARENTHESIS)) {
			tokens.head.get match {
				case identToken: IdentifierToken => tree.children = tree.children :+ SimpleExpression.evaluate(tokens) //args
				case identToken: IntToken => tree.children = tree.children :+ SimpleExpression.evaluate(tokens) //args
				case identToken: DoubleToken => tree.children = tree.children :+ SimpleExpression.evaluate(tokens) //args
				case identToken: StringToken => tree.children = tree.children :+ SimpleExpression.evaluate(tokens) //args
				case separateToken: SeparateToken => separateToken.value match {
					case COMMA => tokens.next()
					case _ => throw new IllegalArgumentException
				}
				case _ => throw new IllegalArgumentException
			}
		}
		if (!tokens.hasNext) {
			throw new IllegalArgumentException
		}
		tokens.next() //left parenthesis
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

case object CompoundStatement extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(CompoundStatement)
		if (tokens.hasNext) {
			return tokens.head.get match {
				case reserved: ReservedToken => reserved.value match {
					case IF =>
						tokens.next()
						tree.children = tree.children :+ Condition.evaluate(tokens)
						tree.children = tree.children ::: CompoundStatement.evaluate(tokens).children
						tree
					case ELSE => tree
					case END => tree
					case _ => throw new IllegalArgumentException
				}
				case ident: IdentifierToken =>
					tree.children = tree.children :+ Expression.evaluate(tokens)
					tree.children = tree.children ::: CompoundStatement.evaluate(tokens).children
					tree
				case _ => throw new IllegalAccessException
			}
		}
		tree
	}
}

case object Condition extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(Condition)
		tree.children = tree.children :+ Expression.evaluate(tokens)
		if (tokens.hasNext) {
			return tokens.head.get match {
				case reservedToken: ReservedToken => reservedToken.value match {
					case THEN | ELSE =>
						tokens.next()
						tree.children = tree.children :+ CompoundStatement.evaluate(tokens)
						tree.children = tree.children ::: Expression.evaluate(tokens).children
						tree
					case _ => throw new IllegalArgumentException
				}
				case separateToken: SeparateToken => separateToken.value match {
					case SEMICOLON =>
						tokens.next()
						tree
					case _ => throw new IllegalArgumentException
				}
				case _ => throw new IllegalArgumentException
			}
		}
		tree
	}
}

case object Function extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(Function)
		tree.children = tree.children :+ FunctionSignature.evaluate(tokens)
		tree.children = tree.children :+ ProgramBody.evaluate(tokens)
		tree
	}
}

case object FunctionSignature extends NonTerminal {

	override def evaluate(tokens: BufferedIterator[Option[Token[_]]]): Tree[Token[_]] = {
		val tree: Tree[Token[_]] = new Tree(FunctionSignature)
		tree.children = tree.children :+ new Tree[Token[_]](Term, List.empty, tokens.next()) //func name
		tokens.head.get match {
			case sepToken: SeparateToken => sepToken.value match {
				case LEFT_PARENTHESIS =>
					tokens.next()
					tree.children = tree.children :+ Var.evaluate(tokens) //args
				case _ => throw new IllegalArgumentException
			}
			case _ => throw new IllegalArgumentException
		}
		if (!tokens.next().get.value.equals(RIGHT_PARENTHESIS)) {
			throw new IllegalArgumentException
		}
		if (!tokens.next().get.value.equals(COLON)) {
			throw new IllegalArgumentException
		}
		tokens.head.get match {
			case typeToken: TypeToken => tree.children = tree.children :+ new Tree[Token[_]](Term, List.empty, tokens.next()) //return type
			case _ => throw new IllegalArgumentException
		}
		if (!tokens.next().get.value.equals(SEMICOLON)) {
			throw new IllegalArgumentException
		}
		tree
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
		val operand = FunctionCallOrIdentifier.evaluate(tokens)
		tree.children = tree.children :+ operand
		if (tokens.hasNext) {
			return tokens.head.get match {
				case operator: OperatorToken => operator.value match {
					case MULTIPLY | DIVIDE =>
						tree.children = tree.children :+ new Tree[Token[_]](Term, List.empty, tokens.next())
						tree.children = tree.children :+ FunctionCallOrIdentifier.evaluate(tokens)
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
