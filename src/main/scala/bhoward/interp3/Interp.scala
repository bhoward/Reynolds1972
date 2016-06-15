package bhoward.interp3

import bhoward.interp._

import scala.util.control.TailCalls._

object Interp extends Interp {
  def apply(a: Exp): Val = eval(a, Init, Fin).result

  def eval(a: Exp, e: Env, k: Cont): TailRec[Val] = a match {
    case IntConst(n) =>
      tailcall(cont(IntVal(n), k))
    case BoolConst(b) =>
      tailcall(cont(BoolVal(b), k))
    case Var(name) =>
      tailcall(cont(e.get(name), k))
    case aa @ Appl(opr, opnd) =>
      tailcall(eval(opr, e, EvOpn(aa, e, k)))
    case aa @ Lambda(param, body) =>
      tailcall(cont(Closure(aa, e), k))
    case aa @ Cond(premise, conclusion, alternative) =>
      tailcall(eval(premise, e, Branch(aa, e, k)))
    case aa @ LetRec(dvar, dexp, body) =>
      tailcall(eval(body, Rec(aa, e), k))
    case Escp(escv, body) =>
      tailcall(eval(body, Simp(escv, Escf(k), e), k))
  }

  def cont(v: Val, k: Cont): TailRec[Val] = k match {
    case Fin =>
      done(v)
    case EvOpn(ap, en, next) =>
      tailcall(eval(ap.opnd, en, ApFun(v, next)))
    case ApFun(fun, next) =>
      tailcall(applyFun(fun, v, next))
    case Branch(cn, en, next) =>
      v match {
        case BoolVal(true) => tailcall(eval(cn.conclusion, en, next))
        case BoolVal(false) => tailcall(eval(cn.alternative, en, next))
        case _ => sys.error("conditional with non-boolean premise")
      }
  }
  
  def applyFun(fun: Val, v: Val, k: Cont): TailRec[Val] = fun match {
    case Closure(lam, en) =>
      tailcall(eval(lam.body, Simp(lam.param, v, en), k))
    case Succ =>
      tailcall(cont(v.succ, k))
    case Pred =>
      tailcall(cont(v.pred, k))
    case Eq1 =>
      tailcall(cont(Eq2(v), k))
    case Eq2(arg) =>
      tailcall(cont(arg.equal(v), k))
    case Plus1 =>
      tailcall(cont(Plus2(v), k))
    case Plus2(arg) =>
      tailcall(cont(arg.plus(v), k))
    case Minus1 =>
      tailcall(cont(Minus2(v), k))
    case Minus2(arg) =>
      tailcall(cont(arg.minus(v), k))
    case Times1 =>
      tailcall(cont(Times2(v), k))
    case Times2(arg) =>
      tailcall(cont(arg.times(v), k))
    case Divide1 =>
      tailcall(cont(Divide2(v), k))
    case Divide2(arg) =>
      tailcall(cont(arg.divide(v), k))
    case Escf(cn) =>
      tailcall(cont(v, cn))
    case _ =>
      sys.error("apply of a non-function")
  }
}
