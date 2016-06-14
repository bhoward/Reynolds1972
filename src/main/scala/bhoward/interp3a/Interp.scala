package bhoward.interp3a

import bhoward.interp._

import scala.annotation.tailrec

object Interp extends Interp {
  def apply(a: Exp): Val = eval(Left(a), Init, Fin)
  
  // The Either[Exp, Val] is to allow eval(exp, ...) and cont(val, ...)
  // to be combined in a single tail-recursive function.
  // TODO compare this to the SECD machine
  @tailrec
  def eval(a: Either[Exp, Val], e: Env, k: Cont): Val = a match {
    case Left(IntConst(n)) =>
      eval(Right(IntVal(n)), e, k)
    case Left(BoolConst(b)) =>
      eval(Right(BoolVal(b)), e, k)
    case Left(Var(name)) =>
      eval(Right(e.get(name)), e, k)
    case Left(aa @ Appl(opr, opnd)) =>
      eval(Left(opr), e, EvOpn(aa, e, k))
    case Left(aa @ Lambda(param, body)) =>
      eval(Right(Closure(aa, e)), e, k)
    case Left(aa @ Cond(premise, conclusion, alternative)) =>
      eval(Left(premise), e, Branch(aa, e, k))
    case Left(aa @ LetRec(dvar, dexp, body)) =>
      eval(Left(body), Rec(aa, e), k)
    case Left(Escp(escv, body)) =>
      eval(Left(body), Simp(escv, Escf(k), e), k)
    case Right(v) =>
      k match {
        case Fin =>
          v
        case EvOpn(ap, en, next) =>
          eval(Left(ap.opnd), en, ApFun(v, next))
        case ApFun(fun, next) =>
          fun match {
            case Closure(lam, en) =>
              eval(Left(lam.body), Simp(lam.param, v, en), next)
            case Succ =>
              eval(Right(v.succ), e, next)
            case Eq1 =>
              eval(Right(Eq2(v)), e, next)
            case Eq2(arg) =>
              eval(Right(arg.equal(v)), e, next)
            case Escf(cn) =>
              eval(Right(v), e, cn)
            case _ =>
              sys.error("apply of a non-function")
          }
        case Branch(cn, en, next) =>
          v match {
            case BoolVal(true) => eval(Left(cn.conclusion), en, next)
            case BoolVal(false) => eval(Left(cn.alternative), en, next)
            case _ => sys.error("conditional with non-boolean premise")
          }
      }
  }
}
