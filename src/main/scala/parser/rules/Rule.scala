package parser.rules

import lexical.tokens.Token
import parser.utils.Tree

import scala.collection.mutable.Stack

/**
	* Created by wannabe on 05.01.16.
	*/
abstract class Rule extends ((Tree[Token[_]], Option[Token[_]], Stack[Tree[Token[_]]]) => Tree[Token[_]]){

}
