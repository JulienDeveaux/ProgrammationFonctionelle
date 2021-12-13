abstract class Tree

case class Sum(l: Tree, r: Tree) extends Tree
case class Var(n: String) extends Tree
case class Const(v: Int) extends Tree

//type Environment = String => Int // fonction qui renvoit la valeur de la chaine

object Calc {
	type Environment = String => Int

	def eval(t: Tree, env: Environment):Int = {
		t match {
			case Sum(a, b) => eval(a, env) + eval(b, env)
			case Var(a) => env(a)
			case Const(a) => a
		}
	}

	def derive(t: Tree, v: String):Tree = {
		t match {
			case Sum(a, b) => Sum(derive(a, v), derive(b, v))
			case Var(a) => if(a == v) { Const(1) } else { Const(0) }
			case Const(a) => Const(0)
			
			//case Var(a) if(a == v) => Const(1) => case plus précis
			//case_ => Const(0) => le reste des cas
		}
	}

	def main(args: Array[String]) = {
		val env: Environment = { case "x" => 5 case "y" => 6 }
		val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y"))) // (x+x)+(7+y)
	
		println("expression : " + exp)
		println("derivée : " + derive(exp, "x"))
		println("res : " + eval(exp, env))
		println("resd : " + eval(derive(exp, "x"), env))
	}
}




/*
scala> :load Tree.scala
val args: Array[String] = Array()
Loading Tree.scala...
class Tree
class Sum
class Var
class Const

scala> val const = Const(5)
val const: Const = Const(5)

scala> const.v
val res0: Int = 5

scala> val const2 = Const(5)
val const2: Const = Const(5)

scala> const == const2
val res1: Boolean = true

scala> const eq const2 // compare les références
val res2: Boolean = false

scala> const ne const2
val res3: Boolean = true

scala> const.v = 6 // c'est que des constantese dans les case class
               ^
       error: reassignment to val

scala> :load Tree.scala
val args: Array[String] = Array()
Loading Tree.scala...
class Tree
class Sum
class Var
class Const
type Environment

scala> val env: Environment = { case "x" => 5 case "y" => 6 }
val env: Environment = $Lambda$1302/0x00000008011d3840@2459715c

scala> env("x")
val res4: Int = 5

*/