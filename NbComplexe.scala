class Complexe(real: Double, imaginary: Double) {
	def re = real		// une seule ligne ça return le real

	def im() = imaginary

	override def toString() = "" + real + (if(imaginary<0) "" else " + ") + imaginary + "i"
}

object NbComplexe {
	def main(args: Array [String]) = {
		val complexe = new Complexe(1.5, 2.3)
		println(complexe.im())
		println(complexe)
	}
}