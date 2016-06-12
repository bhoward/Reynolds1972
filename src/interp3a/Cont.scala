package interp3a

trait Cont

case object Fin extends Cont

case class EvOpn(ap: Appl, en: Env, next: Cont) extends Cont

case class ApFun(fun: Val, next: Cont) extends Cont

case class Branch(cn: Cond, en: Env, next: Cont) extends Cont
