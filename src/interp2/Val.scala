package interp2

trait Val {
  def apply(a: Val): Val = sys.error("apply of a non-function")
}

case class IntVal(n: Int) extends Val

case class BoolVal(b: Boolean) extends Val

trait FunVal extends Val

case class Closure(lam: Lambda, en: Env) extends FunVal {
  override def apply(a: Val): Val = lam.body.eval(Simp(lam.param, a, en))
}

case object Succ extends FunVal {
  override def apply(a: Val): Val = succ(a)
  
  def succ(a: Val): Val = a match {
    case IntVal(n) => IntVal(n + 1)
    case _ => sys.error("succ applied to non-integral argument")
  }
}

case object Eq1 extends FunVal {
  override def apply(a: Val): Val = Eq2(a)
}

case class Eq2(arg: Val) extends FunVal {
  override def apply(a: Val): Val = equal(arg, a)
  
  def equal(a: Val, b: Val): Val = (a, b) match {
    case (IntVal(n1), IntVal(n2)) => BoolVal(n1 == n2)
    case (BoolVal(b1), BoolVal(b2)) => BoolVal(b1 == b2)
    case _ => sys.error("equal applied to incompatible arguments")
  }
}
