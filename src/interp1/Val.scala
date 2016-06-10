package interp1

trait Val {
  def apply(x: Val): Val = sys.error("apply of a non-function")
}

case class IntVal(n: Int) extends Val

case class BoolVal(b: Boolean) extends Val

case class FunVal(fun: Val => Val) extends Val {
  override def apply(x: Val): Val = fun(x)
}
