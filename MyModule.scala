object MyModule {
	
	def abs(n:Int):Int = 
		if(n < 0) -n else n

	def factorial(n:Int):Int = {
		@annotation.tailrec		// le compilateur préviens si c'est pas tail recursive et essai de le faire
		def loop(n:Int, acc:Int):Int = if(n <= 0) acc else loop(n-1, n*acc)

		loop(n, 1)
	}		// terminal recursive (dernier truc dans la fonction c'est l'appel récursif)
			//-> aussi efficace qu'une boucle

	def fib(n:Int):Int = {
		@annotation.tailrec
		def loop(pos:Int, pre1:Int, pre2:Int):Int = {
			if(pos > n - 3) {
				pre1+pre2				
			} else {
				loop(pos + 1, pre2, (pre1 + pre2))
			}
		}
		loop(0, 0, 1)
	}

	def lessThan = new Function2[Int, Int, Boolean] { //2 paramètres le troisième est le retour
		def apply(a:Int, b:Int) = a < b //apply veut dire que l'ont peut l'utiliser comme une fonction
	}									//même si c'est un objet

	def isSorted[A](a:Array[A], gt:(A, A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(it:Int):Boolean = {
			if(it < a.length - 1) {
				if(gt(a(it), a(it + 1))) {
					loop(it + 1)
				} else {
					return false
				}
			} else {
				return true
			}
		}
		loop(0)
	}

	//fonction monomorphique
	def findFirst(seq:Array[String], key:String): Int = {
		@annotation.tailrec
		def loop(n:Int):Int =
			if(n >= seq.length) - 1
			else if(seq(n) == key) n
			else loop(n + 1)
		loop(0)
	}

	//version polymorphique
	def findFirst2[A](s:Array[A], p:A => Boolean /*lambda pour savoir le ==*/):Int = {
		@annotation.tailrec
		def loop(n:Int):Int =
			if(n >= s.length) - 1
			else if (p(s(n))) n
			else loop(n + 1)
		loop(0)
	}

	def partial1[A, B, C](a:A, f:(A, B) => C): B => C = {
		return (b) => f(a, b)
	}

	def curry[A, B, C](f:(A, B) => C) : A => (B => C) = {
		return (a) => b => f(a, b)
	}

	def uncurry[A, B, C](f: A => B => C):(A, B) => C = {
		return (a, b) => f(a)(b)
	}

	private def formatAbs(x:Int) = {
		val msg = "The absolute value of %d is %d"
		msg.format(x, abs(x))
	}

	def formatResult(name:String, n:Int, f:Int => Int) = {
		"The %s of %d is %d".format(name, n, f(n))
	}

	def main(args:Array[String]):Unit = {
		val a = Array(1, 2, 3, 4, 5)
		println(findFirst2[Int](a, (x) => x==3))
		println(isSorted[Int](a, (x, y) => x <= y))
		val b = Array(1, 2, 3, 4, 5, 4)
		println(isSorted[Int](b, (x, y) => x <= y))

		val f = partial1("Prefixe_", (x:String, y:Int) => x + y)
		println(f(45))

		val c = curry((x:String, y:Int) => x + y)
		println(c("Prefixe_")(42))

		val u = uncurry((x:String) => (y:Int) => x + y)
		println(u("Prefixe_", 44))

		println(formatAbs(-42))
		println(formatResult("factorial", 5, factorial))
		println(formatResult("fib", 8, fib))
		println(formatResult("Increment", 7, (x:Int)=> x + 1))
		println(formatResult("Increment", 7, x => x + 1))		// Type déja défini dans formatResult
		println(formatResult("Increment", 7, _ + 1))
	}
}