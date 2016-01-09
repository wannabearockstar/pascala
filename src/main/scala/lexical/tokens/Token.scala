package lexical.tokens

/**
	* Created by wannabe on 29.12.15.
	*/
abstract class Token[T](val line: Int, val position: Int, val text: String) {
	val value: T

	override def toString: String = {
		val tokenName = this.getClass.getSimpleName.replace("Token", "")
		s"$tokenName($value)"
	}
}
