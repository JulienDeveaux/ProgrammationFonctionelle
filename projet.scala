sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right:Tree[A]) extends Tree[A]

case class SimpleRNG(seed:Long) {
	import BinaryTree._
	def nextInt:(Int, SimpleRNG) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >>> 16).toInt
		(n, nextRNG)
	}

	def nonNegativeInt(rng:SimpleRNG):(Int, SimpleRNG) = {
		val (i, r) = rng.nextInt
		(if(i <0) -(i+1) else i, r)
	}

	def rand[A](tree:Tree[A]):(A, SimpleRNG) = {
		val (pos, rng2) = nonNegativeInt(this)		// on récupère une valeur non négative pour l'utiliser comme un numéro de branche dans notre arbre mais ce nombre est très probablement au dessus du nombre de noeud de notre arbre
		var resultdiv = pos % (count(tree)+1)		// le reste d'une division euclidienne étant forcément inférieur à son diviseur, resultDiv est ainsi forcément entre 0 et le nombre de noeuds dans notre arbre
		val (res, b) = fold(tree)(x=> {
			resultdiv = resultdiv - 1
			(x, resultdiv==0)						// Ceci est un tuple contenant (la valeur du noeud, un booléen true si la feuille est en bonne position)
		})((x, y)=> {
			if(y._2 == true) {						// Si le booléen de y est true on le renvoi
				y
			} else {								// Sinon on renvoi x qui peut soit être true ou false, cela importe peu -> on pourrait faire "else if(x._2 == true) { x } else { x }"
				x
			}
		})
		(res, rng2)
	}

	def rand2[A](tree:Tree[A]):(A, SimpleRNG) = {			// Utilisation de la même logique de sélection de la feuille que pour le premier rand mais sans le fold
		val (pos, rng2) = nonNegativeInt(this)
		var resultdiv = pos % (count(tree)+1)				// Nous récupérons le même reste de division euclidienne qui nous servira de position de feuille à retourner
		val treeInList = toList(tree)						// Ici, nous transformons nos feuilles d'arbre en liste chainée pour pouvoir plus facilement le manipuler
		if(treeInList.length > resultdiv) {
			(treeInList(resultdiv), rng2)					// Ici, si l'abre / la liste n'est pas vide, nous renvoyont la valeur de la feuille en position de notre reste de division
		} else {
			(treeInList(0), rng2)							// Sinon, si l'abre / la liste est vide, nous renvoyont la seule valeur de l'abre, sa racine
		}
	}

	def rand3[A](tree:Tree[A]):(A, SimpleRNG) = {  												// Même principe que le premier rand mais en moins brouillon
		val (pos, rng2) = nonNegativeInt(this)
		val resultdiv = pos % (count(tree)+1)													// Nous prenons toujours le résultat de notre reste de division pour avoir notre position, recalculé à chaque fois que la fonction est appelée
		tree match {																			// Mais cette fois nous appliquons une logique différent epour déterminer quelle feuille choisir
			case Branch(l, r) => if(resultdiv%2==0) { this.rand(l) } else { this.rand(r) }		// Ici, nous réappelons la fonction soit sur la partie gauche de la branche soit sur la partie droite en fonction de si notre reste est pair
			case Leaf(l) => (l, rng2)															// Ce qui, à la place de choisir une position de noeud, permet de choisir un chemin pour y accéder
		}
	}
}

object BinaryTree {
	def size[A](tree:Tree[A]):Int = tree match {
		case Branch(l, r) => (size(l) + size(r) + 1)			//Lors ce que l'on fait size + size, nous ne comptons pas les branches mais les leaf qui compte pour 1, il faut donc ajouter 1 à chaque fois que l'on fait ce calcul pour ajouter les branches
		case Leaf(l) => 1
	}

	def count[A](tree:Tree[A]):Int = tree match {
		case Branch(l, r) => (count(l) + count(r))
		case Leaf(l) => 1
	}

	def max(tree:Tree[Int]):Int = tree match {
		case Branch(l, r) => max(l).max(max(r))
		case Leaf(l) => l
	}

	def depth[A](tree:Tree[A]):Int = tree match {
		case Branch(l, r) => (depth(l)+1).max(depth(r)+1)		//Même explication du +1 que pour la fonction size
		case Leaf(l) => 1
	}

	def map[A](tree:Tree[A])(f:A=>A):Tree[A] = tree match {
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
		case Leaf(l) => Leaf(f(l))								//On applique ici via f une opération sur les chaques feuilles
	}

	def fold[A, B](tree:Tree[A])(fl:A=>B)(fb:(B, B) => B):B = tree match {		// 9.Non elle ne peut pas être récursive terminale parce que la dernière fonction appelée sera toujours fl et non fold -> il faut applique la fonction après avoir évalué touts les éléments plus "loin" dans l'arbre
		case Branch(l, r) => fb(fold(l)(fl)(fb), fold(r)(fl)(fb))				// De plus, il n'y a pas de cas ou l'on ne renvoit qu'une simple valeur directement, il faut forcément appliquer la fonction sur la feuille
		case Leaf(l) => fl(l)													// à cause de cela, même si l'on appel fold avant fb, il ne pourra jamais y avoir fold en dernier appel de fonction.
	}

	def size2[A](tree:Tree[A]):Int = {							//***********************************************************/
		fold(tree)(x=>1)((x, y) => (x + y + 1))					//		Comparaison entre l'utilisation du fold et du match : 
	}															//
																// Ici, dans la première lambda, x=>truc représente : case Leaf(x) => truc où truc peut être une fonction ou une simple valeur
	def count2[A](tree:Tree[A]):Int = {							//    la fonction truc représente donc une opération à réaliser sur la valeur d'une feuille / toutes les feuilles de l'arbre
		fold(tree)(x=>1)((x, y) => (x + y))						//
	}															// La seconde lambda contient deux variables. Pour un couple x et y, on applique une fonction f
																//    f représente l'opération à réaliser pour associer / manipuler deux branches
	def max2(tree:Tree[Int]):Int = {							//
		fold(tree)(x=>x)((x, y) => x.max(y))					//
	}															// Ainsi, pour transformer une fonction utilisant match en fold, il suffit de transformer l'opération réalisée à droite des flèches des cases
																// en lamdbas
	def depth2[A](tree:Tree[A]):Int = {							//
		fold(tree)(x=>1)((x, y)=> (x+1).max(y+1))				//
	}															//***********************************************************/

	def map2[A](tree:Tree[A])(f:A=>A):Tree[A] = {
		fold[A, Tree[A]](tree)(x=> Leaf(f(x)))((x, y)=> Branch(x, y))
	}

	def toList[A](tree:Tree[A]):List[A] = {						// Fonction utilisé pour la deuxième version du random
		fold(tree)(x => List(x))((x, y) => x ++ y)				// Elle récupère la valeur des feuilles et les concatène dans une List
	}
}

object TestBinaryTree {
	import BinaryTree._
	import SimpleRNG._
	def main(args:Array[String]): Unit = {
		val arbre = 
		Branch(
			Branch(Leaf(1), Leaf(7))
			,Branch(
				Branch(Leaf(2), Leaf(2))
				,Leaf(7)
			)
		)
		println("arbre utilisé : " + arbre + "\n")
		println("taille de l'arbre : " + size(arbre))
		println("nombre de feuilles : " + count(arbre))
		println("val maximale des feuille : " + max(arbre))
		println("profondeur de l'arbre : " + depth(arbre))
		println("map de l'abre en x2 : " + map(arbre)(x=>x*2))
		println("fold recontruit l'arbre" + fold[Int, Tree[Int]](arbre)(x => Leaf(x))((x, y) => Branch(x, y)))
		println("taille de l'arbre 2 : " + size2(arbre))
		println("nombre de feuilles 2 : " + count2(arbre))
		println("val maximale des feuille 2 : " + max2(arbre))
		println("profondeur de l'arbre 2 : " + depth2(arbre))
		println("map de l'abre en x2 2 : " + map2(arbre)(x=>x*2))
		println("test toList : " + toList(arbre) + "\n")

		val rng = new SimpleRNG(System.currentTimeMillis())
		val arbreRandom = 
		Branch(
			Branch(
				Branch(
					Branch(Leaf(1), Leaf(2))
				, Branch(Leaf(3), Leaf(4))
			), Leaf(5))
		, Leaf(6))
		println("test du random (la graine change entre chaque exécution)")
		println("arbre utilisé : " + arbre + "\n")
		println("Test random même graine : " + rng.rand[Int](arbreRandom))
		println("Test random même graine : " + rng.rand[Int](arbreRandom))
		println("Test random arbre vide : " + rng.rand[Int](Leaf(0)))
		println("")
		println("Test random2 même graine : " + rng.rand2[Int](arbreRandom))
		println("Test random2 même graine : " + rng.rand2[Int](arbreRandom))
		println("Test random2 arbre vide : " + rng.rand2[Int](Leaf(0)))
		println("")
		println("Test random3 même graine : " + rng.rand3[Int](arbreRandom))
		println("Test random3 même graine : " + rng.rand3[Int](arbreRandom))
		println("Test random3 arbre vide : " + rng.rand3[Int](Leaf(0)))
		println("")
	}
}