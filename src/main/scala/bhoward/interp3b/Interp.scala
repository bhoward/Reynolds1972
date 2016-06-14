package bhoward.interp3b

import bhoward.interp._

import scala.annotation.tailrec

object Interp extends Interp {
  def apply(a: Exp): Val = eval(E, a, Init, Fin)
  
  sealed trait Fun
  case object E extends Fun
  case object C extends Fun

  // eval(E, a, e, k) is eval(a, e, k);
  // eval(C, v=v, k=k) is cont(v, k)
  @tailrec
  def eval(fun: Fun, a: Exp = null, e: Env = null, k: Cont = null, v: Val = null): Val = fun match {
    case E =>
      a match {
        case IntConst(n) =>
          eval(C, v=IntVal(n), k=k)
        case BoolConst(b) =>
          eval(C, v=BoolVal(b), k=k)
        case Var(name) =>
          eval(C, v=e.get(name), k=k)
        case aa @ Appl(opr, opnd) =>
          eval(E, opr, e, EvOpn(aa, e, k))
        case aa @ Lambda(param, body) =>
          eval(C, v=Closure(aa, e), k=k)
        case aa @ Cond(premise, conclusion, alternative) =>
          eval(E, premise, e, Branch(aa, e, k))
        case aa @ LetRec(dvar, dexp, body) =>
          eval(E, body, Rec(aa, e), k)
        case Escp(escv, body) =>
          eval(E, body, Simp(escv, Escf(k), e), k)
      }
    case C =>
      k match {
        case Fin =>
          v
        case EvOpn(ap, en, next) =>
          eval(E, ap.opnd, en, ApFun(v, next))
        case ApFun(fun, next) =>
          fun match {
            case Closure(lam, en) =>
              eval(E, lam.body, Simp(lam.param, v, en), next)
            case Succ =>
              eval(C, v=v.succ, k=next)
            case Eq1 =>
              eval(C, v=Eq2(v), k=next)
            case Eq2(arg) =>
              eval(C, v=arg.equal(v), k=next)
            case Escf(cn) =>
              eval(C, v=v, k=cn)
            case _ =>
              sys.error("apply of a non-function")
          }
        case Branch(cn, en, next) =>
          v match {
            case BoolVal(true) => eval(E, cn.conclusion, en, next)
            case BoolVal(false) => eval(E, cn.alternative, en, next)
            case _ => sys.error("conditional with non-boolean premise")
          }
      }
  }
}
