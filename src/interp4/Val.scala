package interp4

import scala.util.control.TailCalls._

trait Val {
  def apply(x: Val, k: Cont): TailRec[Val] = sys.error("apply of a non-function")
}

case class IntVal(n: Int) extends Val

case class BoolVal(b: Boolean) extends Val

case class FunVal(fun: (Val, Cont) => TailRec[Val]) extends Val {
  override def apply(x: Val, k: Cont): TailRec[Val] = tailcall(fun(x, k))
}
