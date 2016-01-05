package parser.utils

/**
	* Created by wannabe on 05.01.16.
	*/
class Tree[A](var v: Option[A], var l: Option[Tree[A]], var r: Option[Tree[A]]) {
}
