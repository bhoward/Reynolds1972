package interp2

import interp._

object Interp extends Interp {
  def apply(a: Exp): Val = eval(a, Init)

  def eval(a: Exp, e: Env): Val = a match {
    case IntConst(n) =>
      IntVal(n)
    case BoolConst(b) =>
      BoolVal(b)
    case Var(name) =>
      e.get(name)
    case Appl(opr, opnd) =>
      applyFun(eval(opr, e), eval(opnd, e))
    case aa @ Lambda(param, body) =>
      Closure(aa, e)
    case Cond(premise, conclusion, alternative) =>
      eval(premise, e) match {
        case BoolVal(true) => eval(conclusion, e)
        case BoolVal(false) => eval(alternative, e)
        case _ => sys.error("conditional with non-boolean premise")
      }
    case aa @ LetRec(dvar, dexp, body) =>
      eval(body, Rec(aa, e))
    case Escp(escv, body) =>
      sys.error("escape operation not available")
  }
  
  def applyFun(fun: Val, v: Val): Val = fun match {
    case Closure(lam, en) =>
      eval(lam.body, Simp(lam.param, v, en))
    case Succ =>
      v.succ
    case Eq1 =>
      Eq2(v)
    case Eq2(arg) =>
      arg.equal(v)
    case _ =>
      sys.error("apply of non-function")
  }
}
