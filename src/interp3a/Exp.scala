package interp3a

import scala.annotation.tailrec

trait Exp {
  def interpret: Val = Exp.eval(this, Init, Fin)
}

object Exp {
  // TODO compare this to the SECD machine
  @tailrec
  def eval(a: Exp, e: Env, k: Cont): Val = a match {
    case IntConst(n) =>
      eval(AppCont(IntVal(n)), e, k)
    case BoolConst(b) =>
      eval(AppCont(BoolVal(b)), e, k)
    case Var(name) =>
      eval(AppCont(e.get(name)), e, k)
    case Appl(opr, opnd) =>
      eval(opr, e, EvOpn(a.asInstanceOf[Appl], e, k))
    case Lambda(param, body) =>
      eval(AppCont(Closure(a.asInstanceOf[Lambda], e)), e, k)
    case Cond(premise, conclusion, alternative) =>
      eval(premise, e, Branch(a.asInstanceOf[Cond], e, k))
    case LetRec(dvar, dexp, body) =>
      eval(body, Rec(a.asInstanceOf[LetRec], e), k)
    case AppCont(v) =>
      k match {
        case Fin =>
          v
        case EvOpn(ap, en, next) =>
          eval(ap.opnd, en, ApFun(v, next))
        case ApFun(fun, next) =>
          fun match {
            case Closure(lam, en) =>
              eval(lam.body, Simp(lam.param, v, en), next)
            case Succ =>
              eval(AppCont(succ(v)), e, next)
            case Eq1 =>
              eval(AppCont(Eq2(v)), e, next)
            case Eq2(arg) =>
              eval(AppCont(equal(arg, v)), e, next)
            case _ =>
              sys.error("apply of a non-function")
          }
        case Branch(cn, en, next) =>
          v match {
            case BoolVal(true) => eval(cn.conclusion, en, next)
            case BoolVal(false) => eval(cn.alternative, en, next)
            case _ => sys.error("conditional with non-boolean premise")
          }
      }
  }

  def succ(a: Val): Val = a match {
    case IntVal(n) => IntVal(n + 1)
    case _ => sys.error("succ applied to non-integral argument")
  }

  def equal(a: Val, b: Val): Val = (a, b) match {
    case (IntVal(n1), IntVal(n2)) => BoolVal(n1 == n2)
    case (BoolVal(b1), BoolVal(b2)) => BoolVal(b1 == b2)
    case _ => sys.error("equal applied to incompatible arguments")
  }
}

case class IntConst(n: Int) extends Exp

case class BoolConst(b: Boolean) extends Exp

case class Var(name: String) extends Exp

case class Appl(opr: Exp, opnd: Exp) extends Exp

case class Lambda(param: Var, body: Exp) extends Exp

case class Cond(premise: Exp, conclusion: Exp, alternative: Exp) extends Exp

case class LetRec(dvar: Var, dexp: Lambda, body: Exp) extends Exp

case class AppCont(v: Val) extends Exp
