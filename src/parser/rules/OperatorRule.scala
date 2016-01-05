package parser.rules

import lexical.tokens.Token
import parser.utils.Tree

import scala.collection.mutable

/**
	* Created by wannabe on 05.01.16.
	*/
class OperatorRule extends Rule {

	override def apply(node: Tree[Token[_]], token: Option[Token[_]], stack: mutable.Stack[Tree[Token[_]]]): Tree[Token[_]] = {
		node.v = Some(token.get)
		node.r = Some(new Tree(None, None, None))
		stack.push(node)
		node.r.get
	}
}
