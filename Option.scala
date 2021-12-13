import scala.collection.immutable.HashMap

sealed trait Option[+A] {
	def mean(xs:Seq[Double]):Option[Double] = {
		if(xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	def map[B](f:A => B):Option[B] = {
		this match {
			case Some(a) => Some(f(a))
			case None => None
		}
	}

	def getOrElse[B >: A](default: => B):B = {
		 this match {
		 	case Some(a) if(a != None) => a
		 	case _ => default
		 }
	}

	def flatMap[B](f:A => Option[B]):Option[B] = {
		this match {
			case Some(a) => f(a)
			case _ => None
		}
	}

	def orElse[B >: A](default: => Option[B]):Option[B] = {
		this match {
			case Some(a) if(a != None) => Some(a)
			case _ => default
		}
	}

	def filter(f:A => Boolean):Option[A] = {
		this match {
			case Some(a) if(f(a))=> None
			case Some(a) => Some(a)
			case None => None
		}
	}

	def lift[A, B](f:A => B):Option[A] => Option[B] = _ map f
}
case object None extends Option[Nothing]
case class Some[+A](getS:A) extends Option[A]


object TestOption {
	def map2[A, B, C](a:Option[A], b:Option[B])(f: (A, B) => C):Option[C] = {
		(a, b) match {
			case (Some(a), Some(b)) => Some(f(a, b))
			case _ => None
		}
	}

	def sequence[A](a:List[Option[A]]):Option[List[A]] = {
		a match {
			case Nil => Some(Nil)
			case None :: _ => None
			case Some(h) :: t => sequence(t).map(x=>h::x)
		}
	}

	def main(args:Array[String]):Unit = {
		val hm = Seq(1, 2, 3)
		println("map Test : " + Some(hm).map(x => x + "0.1"))
		println("getOrElse None : " + Some(None).getOrElse(-1))
		println("getOrElse hm : " + Some(hm).getOrElse(-1))
		println("flatMap : " + Some(hm).flatMap(x => Some(x)))
		println("orElse None : " + Some(None).orElse(Some(4)))
		println("orElse : " + Some(hm).orElse(Some(4)))
		println("filter 2 : " + Some(hm).filter(x => x.contains(2)))
		println("filter 22 : " + Some(hm).filter(x => x.contains(22)))


		val absOpt:Option[Double] => Option[Double] = Some(0.1).lift(math.abs)
		println(absOpt)

		println("map2 : " + map2(Some(8), Some(8))((x, y) => x + "_.:._" + y))
		println("sequence : " + sequence(List(Some(1), Some(2), Some(3))))
		println("sequence none : " + sequence(List(Some(1), None, Some(3))))
	}
}