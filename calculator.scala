abstract class Tree
case class Sum(l: Tree, r: Tree) extends Tree {
  override def toString(): String = s"(${l.toString()} + ${r.toString()})"
}
case class Prod(l: Tree, r: Tree) extends Tree {
  override def toString(): String = s"${l.toString()} * ${r.toString()}"
}
case class Const(v: Int) extends Tree { 
  override def toString(): String = v.toString
}
case class Var(v: String) extends Tree {
  override def toString(): String = v
}

object Calculator {
  type Environment = String => Int

  def eval(t: Tree, env: Environment): Int = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Prod(l, r) => eval(l, env) * eval(r, env)
    case Const(v) => v
    case Var(v) => env(v)
  }
  
  def simplify(t: Tree): Tree = t match {
    case Sum(Const(0), r) => simplify(r)
    case Sum(l, Const(0)) => simplify(l)
    case Sum(Const(v1), Const(v2)) => Const(v1 + v2)
    case Prod(l, Const(0)) => Const(0)
    case Prod(Const(0), r) => Const(0)
    case Prod(Const(1), r) => simplify(r)
    case Prod(l, Const(1)) => simplify(l)
    case Prod(l, r) => {
      val l_simp = simplify(l)
      val r_simp = simplify(r)
      if (l != l_simp || r != r_simp)
        simplify(Prod(l_simp, r_simp))
      else
        Prod(l_simp, r_simp)
    }
    case Sum(l, r) => {
      val l_simp = simplify(l)
      val r_simp = simplify(r)
      if (l != l_simp || r != r_simp)
        simplify(Sum(l_simp, r_simp))
      else
        Sum(l_simp, r_simp)
    }
    case Const(v) => Const(v)
    case Var(v) => Var(v)
  }

  def derive(t: Tree, v: String): Tree = {
    def sym_derive(t: Tree, v: String): Tree = t match {
      case Sum(l, r) => Sum(derive(l, v), derive(r, v))
      case Prod(l, r) => Sum(Prod(derive(l, v), r), Prod(l, derive(r, v)))
      case Const(v) => Const(0)
      case Var(n) if (v == n) => Const(1)
      case Var(v) => Const(0)
    }
    simplify(sym_derive(t, v))
  }

  def main(args: Array[String]): Unit = {
    val exp = Sum(
      Prod(Var("x"), Prod(Var("x"), Var("x"))),
      Prod(Const(42), Prod(Var("x"), Var("y"))))
    val env: Environment = {
      case "x" => 2
      case "y" => 5}
    println(s"Expression: $exp")
    println(s"Evaluation with x=2, y=5: ${eval(exp, env)}")
    val exp2 = Sum(
      Prod(Var("x"), Var("x")),
      Prod(Var("x"), Var("y")))
    println(s"Expression: $exp2")
    println(s"Derivative wrt x: ${derive(exp2, "x")}")
    println(s"Derivative wrt y: ${derive(exp2, "y")}")
  }
}
