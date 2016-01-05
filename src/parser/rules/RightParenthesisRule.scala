package parser.rules

import lexical.tokens.Token
import parser.utils.{Tree}

import scala.collection.mutable

/**
	* Created by wannabe on 05.01.16.
	*/
class RightParenthesisRule extends Rule{

	override def apply(v1: Tree[Token[_]], v2: Option[Token[_]], stack: mutable.Stack[Tree[Token[_]]]): Tree[Token[_]] = {
		if (stack.isEmpty) {
			return new Tree[Token[_]](None, None, None)
			//todo handling
		}
		stack.pop()
	}
}
