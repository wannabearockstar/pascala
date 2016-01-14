package parser.utils

import parser.NonTerminal

/**
	* Created by wannabe on 05.01.16.
	*/
class Tree[A](var nonTerminal: NonTerminal, var children: List[Tree[A]] = List.empty, val value: Option[A] = None) {

	def asHorizontalString(tree: Tree[_] = this, levels: Int = 1, last: Boolean = false): String = {
		var cache = ""
		cache += "\t" * levels
		cache += (if (!last) "├── " else "└── ")
		cache += (if (value.isDefined) value.get.toString else tree.nonTerminal.toString) + "\n"
		if (tree.children.nonEmpty) {
			val newLevel = levels + 1
			tree.children.foreach(x => {
				cache += x.asHorizontalString(x, newLevel, tree.children.last == x)
			})
		}
		cache
	}
}
