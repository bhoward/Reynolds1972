package interp3a

trait Val

case class IntVal(n: Int) extends Val

case class BoolVal(b: Boolean) extends Val

trait FunVal extends Val

case class Closure(lam: Lambda, en: Env) extends FunVal

case object Succ extends FunVal

case object Eq1 extends FunVal

case class Eq2(arg: Val) extends FunVal
