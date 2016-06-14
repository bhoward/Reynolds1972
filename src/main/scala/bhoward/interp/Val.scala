package bhoward.interp

sealed trait Val {
  def show: String

  def succ: Val = sys.error("succ applied to non-integral argument")

  def equal(a: Val): Val = sys.error("equal applied to non-base arguments")
}

case class IntVal(n: Int) extends Val {
  def show: String = n.toString
  
  override def succ: Val = IntVal(n + 1)
  
  override def equal(a: Val): Val = a match {
    case IntVal(n1) => BoolVal(n == n1)
    case _ => sys.error("equal applied to incompatible arguments")
  }
}

case class BoolVal(b: Boolean) extends Val {
  def show: String = b.toString
  
  override def equal(a: Val): Val = a match {
    case BoolVal(b1) => BoolVal(b == b1)
    case _ => sys.error("equal applied to incompatible arguments")
  }
}

trait FunVal extends Val {
  def show: String = sys.error("Unable to show function values")
}
