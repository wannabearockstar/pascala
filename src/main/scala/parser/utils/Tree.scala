package parser.utils

/**
	* Created by wannabe on 05.01.16.
	*/
class Tree[A](var v: Option[A], var l: Option[Tree[A]], var r: Option[Tree[A]]) {

	def asHorizontalString(tree: Tree[_] = this, idents: Int = 0, separator: String = "|"): String = {
		var cache = ""
		val newIdents = idents + tree.v.get.toString.length
		if (tree.l.isDefined) {
			cache += asHorizontalString(tree.l.get, newIdents, "/") + "\n"
		}
		cache += (" " * idents) + separator + tree.v.get.toString
		if (tree.r.isDefined) {
			cache += "\n" + asHorizontalString(tree.r.get, newIdents, "\\")
		}
		cache
	}
}
