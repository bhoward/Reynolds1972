package interp3

import scala.util.control.TailCalls._

trait Exp {
  def interpret: Val = eval(Init, Fin).result
  
  def eval(e: Env, k: Cont): TailRec[Val]
}

case class IntConst(n: Int) extends Exp {
  def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(IntVal(n)))
}

case class BoolConst(b: Boolean) extends Exp {
  def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(BoolVal(b)))
}

case class Var(name: String) extends Exp {
  def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(e.get(name)))
}

case class Appl(opr: Exp, opnd: Exp) extends Exp {
  def eval(e: Env, k: Cont): TailRec[Val] = tailcall(opr.eval(e, EvOpn(this, e, k)))
}

case class Lambda(param: Var, body: Exp) extends Exp {
  def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(Closure(this, e)))
}

case class Cond(premise: Exp, conclusion: Exp, alternative: Exp) extends Exp {
  def eval(e: Env, k: Cont): TailRec[Val] = tailcall(premise.eval(e, Branch(this, e, k)))
}

case class LetRec(dvar: Var, dexp: Lambda, body: Exp) extends Exp {
  def eval(e: Env, k: Cont): TailRec[Val] = tailcall(body.eval(Rec(this, e), k))
}