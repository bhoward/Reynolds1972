package interp3

import scala.util.control.TailCalls._

trait Val {
  def apply(a: Val, k: Cont): TailRec[Val] = sys.error("apply of a non-function")
}

case class IntVal(n: Int) extends Val

case class BoolVal(b: Boolean) extends Val

trait FunVal extends Val

case class Closure(lam: Lambda, en: Env) extends FunVal {
  override def apply(a: Val, k: Cont): TailRec[Val] = tailcall(lam.body.eval(Simp(lam.param, a, en), k))
}

case object Succ extends FunVal {
  override def apply(a: Val, k: Cont): TailRec[Val] = tailcall(k(succ(a)))
  
  def succ(a: Val): Val = a match {
    case IntVal(n) => IntVal(n + 1)
    case _ => sys.error("succ applied to non-integral argument")
  }
}

case object Eq1 extends FunVal {
  override def apply(a: Val, k: Cont): TailRec[Val] = tailcall(k(Eq2(a)))
}

case class Eq2(arg: Val) extends FunVal {
  override def apply(a: Val, k: Cont): TailRec[Val] = tailcall(k(equal(arg, a)))
  
  def equal(a: Val, b: Val): Val = (a, b) match {
    case (IntVal(n1), IntVal(n2)) => BoolVal(n1 == n2)
    case (BoolVal(b1), BoolVal(b2)) => BoolVal(b1 == b2)
    case _ => sys.error("equal applied to incompatible arguments")
  }
}
