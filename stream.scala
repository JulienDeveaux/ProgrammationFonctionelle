sealed trait Stream[+A] {
	import Stream._

	//le cas 'Cons(h, t)' n'évalue jamais 't'
	def headOption:Option[A] = this match {
		case Empty => None
		case Cons(h, t) => Some(h())		//on est obligé de l'évaluation donc on appelle la fonction
	}

	def toList: List[A] = this match {
		case Empty => Nil
		case Cons(h, t) => h()::t().toList
	}

	def take(n: Int): Stream[A] = this match {
		case Cons(h, t) if(n>0) => cons(h(), t().take(n-1))
		case Cons(h, t) if(n==0) => cons(h(), Empty)
		case _ => Empty
	}

	def drop(n: Int):Stream[A] = this match {
		case Cons(h, t) if(n>=0) => t().drop(n-1)
		case _ => this
	}

	def exists(p:A => Boolean): Boolean = this match {
		case Cons(h, t) => p(h()) || t().exists(p)
		case _ => false
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {		// ..=>B ou A,.. => B -> les .. c'est un lazy B
		case Cons(h, t) => f(h(), t().foldRight(z)(f))
		case _ => z
	}

	def forAll(f: A => Boolean): Boolean = {
		this.foldRight(true)((h, b) => f(h) && b)
	}
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	val ones: Stream[Int] = Stream.cons(1, ones)

	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty		// type le empty

	def apply[A](as: A*): Stream[A] = {
		if(as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))			// _* -> transforme une séquence en liste d'éléments
	}
}

object TestStream {
	import Stream._
	def main(args:Array[String]): Unit = {
		val list = Stream(1, 2, 3, 4, 5, 6)
		println("head : " + list.headOption)
		println("toList : " + list.toList)
		println("take 2 : " + list.take(2).toList)
		println("drop 2 : " + list.drop(2).toList)
		println("forAll 2 : " + list.forAll(x=>x<6))
	}
}