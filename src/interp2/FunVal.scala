package interp2

import interp._

case class Closure(lam: Lambda, en: Env) extends FunVal

case object Succ extends FunVal

case object Eq1 extends FunVal

case class Eq2(arg: Val) extends FunVal
