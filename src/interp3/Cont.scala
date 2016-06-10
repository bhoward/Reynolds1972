package interp3

import scala.util.control.TailCalls._

trait Cont {
  def apply(a: Val): TailRec[Val]
}

case object Fin extends Cont {
  def apply(a: Val): TailRec[Val] = done(a)
}

case class EvOpn(ap: Appl, en: Env, next: Cont) extends Cont {
  def apply(a: Val): TailRec[Val] = tailcall(ap.opnd.eval(en, ApFun(a, next)))
}

case class ApFun(fun: Val, next: Cont) extends Cont {
  def apply(a: Val): TailRec[Val] = tailcall(fun(a, next))
}

case class Branch(cn: Cond, en: Env, next: Cont) extends Cont {
  def apply(a: Val): TailRec[Val] = a match {
    case BoolVal(true) => tailcall(cn.conclusion.eval(en, next))
    case BoolVal(false) => tailcall(cn.alternative.eval(en, next))
    case _ => sys.error("conditional with non-boolean premise")
  }
}
