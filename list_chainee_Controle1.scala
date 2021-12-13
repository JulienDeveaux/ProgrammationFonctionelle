sealed trait List[+A]	//+A -> covariant pour que l'héritage soit automatique pour les listes

case object Nil extends List[Nothing]

case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {
	def apply[A](as: A*):List[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def sum(ints:List[Int]):Int = ints match {
		case Nil => 0
		case Cons(h, t) => h + sum(t)
	}

	def product(ds:List[Int]):Int = ds match {
		case Nil => 1
		case Cons(0, _) => 0
		case Cons(h, t) => h * product(t)
	}

	def tail[A](l:List[A]):List[A] = {
		l match {
			case Nil => sys.error("Liste vide :/")
			case Cons(_, t) => t
		}
	}

	def drop[A](l:List[A], n:Int):List[A] = {
		if(n==0) return l
		l match {
			case Nil => Nil
			case Cons(h, t) => drop(t, n-1)
		}
	}

	def drop2[A](l:List[A], n:Int):List[A] = {
		l match {
			case Cons(_, t) if(n > 0) => drop(t, n-1)
			case x => x
		}
	}

	def drop3[A](l:List[A], n:Int):List[A] = {
		if(n > 0) drop3(tail(l), n - 1) else l
	}

	def dropWhile[A](l:List[A])(f:A => Boolean): List[A] = {
		l match {
			case Nil => Nil
			case Cons(h, t) => if(f(h)) dropWhile(t)(f) else l
		}
	}

	def dropWhile2[A](l:List[A])(f:A => Boolean): List[A] = {
		l match {
			case Cons(h, t) if(f(h)) => dropWhile(t)(f)
			case x => x
		}
	}

	def setHead[A](l:List[A], newHead:A):List[A] = {
		Cons(newHead, tail(l))
	}

	def append[A](a1:List[A], a2:List[A]):List[A] = a1 match {
		case Nil => a2
		case Cons(h, t) => Cons(h, append(t, a2))
	}

	def init[A](l:List[A]): List[A] = {
		l match {
			case Nil => Nil
			case Cons(h, Nil) => Nil
			case Cons(h, t) => Cons(h, init(t))
		}
	}

	def foldRight[A, B](l:List[A], z:B)(f:(A, B) => B):B = {
		l match {
			case Nil => z
			case Cons(h, t) => f(h, foldRight(t, z)(f))
		}
	}

	def sum2(l:List[Int]) = foldRight(l, 0.0)(_+_)

	def product2(l:List[Double]) = foldRight(l, 1.0)(_*_)

	def length[A](l:List[A]): Int = foldRight(l, 0)((_, y)=> y+1)

	@annotation.tailrec
	def foldLeft[A,B](l:List[A], z:B)(f:(B, A) => B):B = {
		l match {
			case Nil => z
			case Cons(h, t) => foldLeft(t, f(z,h))(f)
		}
	}

	def sum3(l:List[Int]) = foldLeft(l, 0.0)(_-_)

	def product3(l:List[Int]) = foldLeft(l, 1.0)(_*_)

	def length2[A](l:List[A]) = foldLeft(l, 0)((x, _) => x+1)

	def reverseFold[A](l:List[A]) = {
		foldLeft(l, Nil:List[A])((x, y)=>Cons(y, x))
	}

	def foldRight2[A, B](l:List[A], z:B)(f:(A, B) => B): B = {
		foldLeft(reverseFold(l), z)((x, y) => f(y, x))
	}

	def foldLeft2[A, B](l:List[A], z:B)(f:(B, A) => B): B = {
		foldRight(reverseFold(l), z)((x, y) => f(y, x))
	}

	def appendFoldRight[A](l:List[A], l2:List[A]) = {
		foldRight(l, l2)((x, y)=>Cons(x, y))
	}

	def appendFoldLeft[A](l:List[A], l2:List[A]) = {
		foldLeft(reverseFold(l), l2)((x, y)=>Cons(y, x))
	}

	def concat[A](l:List[List[A]]): List[A] = {
		foldRight(l, Nil:List[A])(appendFoldRight _)
	}

	def inc(l:List[Int]):List[Int] = {
		foldRight(l, Nil:List[Int])((x, y) => Cons(x+1, y))
	}

	def doubleString(l:List[Double]):List[String] = {
		foldRight(l, Nil:List[String])((x, y) => Cons(""+x, y))
	}

	def map[A, B](l:List[A])(f:A => B):List[B] = {
		foldRight(l, Nil:List[B])((x, y)=> Cons(f(x), y))
	}

	def filter[A](l:List[A])(f:A => Boolean):List[A] = {
		foldRight(l, Nil:List[A])((x, y)=> if(f(x)) Cons(x, y) else y)
	}

	def flatMap[A](l:List[A])(f:(A, List[A])=>List[A]):List[A] = {
		foldRight(l, Nil:List[A])((x, y)=>f(x, y))
	}

	def tAdd(l1:List[Int], l2:List[Int]):List[Int] = {
		(l1, l2) match {
			case (Cons(h, t), Cons(h2, t2)) => Cons((h + h2), tAdd(t, t2));
			case _ => Nil
		}
	}

	def zip[A](l1:List[A], l2:List[A])(f:(A,A)=>A):List[A] = {
		(l1, l2) match {
			case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zip(t, t2)(f));
			case _ => Nil
		}
	}

	/*def hasSubsequence[A](l:List[A], sub:List[A]):Boolean = {
		var tail = sub
		(l, tail) match {
			case (Cons(h, t), Cons(h2, t2)) if(tail(tail) == t2) => true
			case _ => false
		}
	}*/
																																	/********************** Réponse aux questions ************************/
	def reduce[A](l: List[A], nul:A)(f: (A, A)=>A):A = l match {		// 1.Non elle ne peut pas être récursive terminale parce que l'on appelle jamais reduce() tout seul, il est appelé chaque fois dans f -> f est toujours appelé en dernier
		case Cons(h, Nil) => h 																				// 2.C'est une fonction pure parce que seul les arguments de la fonction déterminent son résultat(pas de paramètres extérieurs à la fonction); un argument donnera toujours un même resultat
		case Cons(h, t) => f(h, reduce(t, nul)(f))										// 3.Cette fonction est d'ordre supérieur parce qu'elle prends en paramètres une fonction ou une lambda
		case _ => nul
	}
}

object TestList {
  import List._

  def main(args: Array[String]) = {
    /*val l = List(1, 3, 2, 4, 3, 4)
    println("l => " + l)
    print("somme de l : " + List.sum(l) + " / ")
    println("produit de l : " + product(l))
    var l2 = Cons(0, l)
    println("l(1, 2, 3, 4) : " + l)
    println("add 0 à l : " + l2)
    println("retire le premier élément de l2 : " + tail(l2))
    println("retire les deux premiers éléments de l : " + drop(l, 2))
    println("retire les deux premiers éléments de l : " + drop2(l, 2))
    println("retire les deux premiers éléments de l : " + drop3(l, 2))
    println("retire les 1 l : " + dropWhile2(l)(x=>x==1))
    println("Ajout 6 en tête de l : " + setHead(l, 6))
    println("init(l) : " + init(l))
    println("sum2(l) : " + sum2(l))
    println("length(l) : " + length(l))
    println("length(\"test\") : " + length(List('t', 'e', 's', 't')))
    println("test foldRight : " + foldRight(List(1,2,3), 0)(_-_))
    println("test foldRight2 : " + foldRight2(List(1,2,3), 0)(_-_))
    println("test foldLeft : " + foldLeft(List(1,2,3), 0)(_-_))
    println("test foldLeft2 : " + foldLeft2(List(1,2,3), 0)(_-_))
    println("test2 foldLeft : " + foldRight(l, "*"){(a, b) => "(" + a + "+" + b + ")"})
    println("test2 foldLeft : " + foldLeft(l, "*"){(a, b) => "(" + a + "+" + b + ")"})
    println("somme foldLeft : " + sum3(List(1,2,3)))
    println("product foldLeft : " + product3(List(1,2,3)))
    println("length foldLeft : " + length2(List(1,2,3)))
    println("test reverseFold : " + reverseFold(List(1,2,3)))
    println("test appendFoldRight : " + appendFoldRight(l, List(1,2,3)))
    println("test appendFoldleft : " + appendFoldLeft(l, List(1,2,3)))
    println("test concat : " + concat(List(l, List(1,2,3))))
    println("test inc : " + inc(l))
    println("test doubleString : " + doubleString(List(0.0, 2.2, 4.4)))
    println("test map : " + map(List(0.0, 2.2, 4.4))(x => x+1))
    println("test filter : " + filter(l)(x => 0==x%2))
    println("test flatMap : " + flatMap(List(1, 2, 3))(((x, y)=>concat(List(List(x, x), y)))))
    println("test tAdd : " + tAdd(List(1, 2, 3), List(4, 5, 6)))
    println("test zip : " + zip(List("1", "2", "3"), List("4", "5", "6"))((x, y)=>(x+"-"+y)))
    //println("test subSequence : " + hasSubsequence(List(1, 2, 3, 2), List(3, 2)))*/
    println("Liste de test : (1, 2, 3, 4)")
    println("reduce string : " + reduce(List("1", "2", "3", "4"), "")((x, y)=> "("+x+"+"+y+")"))
    println("\nOpérations du reduce avec des Int : ")
    println("reduce int (1, 2, 3, 4): " + reduce(List(1, 2, 3, 4), 0)((x, y)=>
    	{
    		println(x + "+" + y + ";")
    		x+y
    	}
    ))
  }
}