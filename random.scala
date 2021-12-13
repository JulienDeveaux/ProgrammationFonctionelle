trait RNG {
	def nextInt:(Int, RNG)
}

case class SimpleRNG(seed:Long) extends RNG {
	def nextInt:(Int, RNG) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >>> 16).toInt
		(n, nextRNG)
	}
}

object TestRnd {
	import SimpleRNG._
	def main(args:Array[String]): Unit = {
		val rng = SimpleRNG(42)
		val (n2, rng2) = rng.nextInt
		println("n2;rng;rng2 : " + n2 + " ; " + rng + " ; " + rng2)
		println(nonNegativeInt(rng))
		println(double(rng))
		println(intDouble(rng))
		println(double3(rng))
		println(ints(5)(rng))
	}

	def nonNegativeInt(rng:RNG):(Int, RNG) = {
		val (i, r) = rng.nextInt
		(if(i <0) -(i+1) else i, r)
	}

	def double(rng:RNG):(Double, RNG) = {
		val(i, r) = nonNegativeInt(rng)
		(i/ Int.MaxValue.toDouble, r)
	}

	def intDouble(rng:RNG):((Int, Double), RNG) = {
		val (i, r) = rng.nextInt
		val(d, r2) = double(r)
		((i, d), r2)
	}

	def double3(rng:RNG):((Double, Double, Double), RNG) = {
		val (d1, r1) = double(rng)
		val (d2, r2) = double(r1)
		val (d3, r3) = double(r2)
		((d1, d2, d3), r3)
	}

	def ints(count:Int)(rng:RNG):(List[Int], RNG) = {
		if(count > 0) {
			val (i, r) = rng.nextInt
			val (ti, tr) = ints(count-1)(r)
			(i::ti, tr)
		} else {
			(List(), rng)
		}
	}
}