package bhoward.interp4

import bhoward.interp._

import scala.util.control.TailCalls._

object Interp extends Interp {
  def apply(a: Exp): Val = eval(a, initEnv, Cont { done(_) }).result

  val initEnv: Env = Env {
    case Var("succ") => FVal {
      case (IntVal(n), k) => tailcall(k(IntVal(n + 1)))
      case _ => sys.error("succ applied to non-integral argument")
    }
    case Var("equal") => FVal {
      case (a, k) => tailcall(k(FVal {
        case (b, k) => tailcall(k((a, b) match {
          case (IntVal(n1), IntVal(n2)) => BoolVal(n1 == n2)
          case (BoolVal(b1), BoolVal(b2)) => BoolVal(b1 == b2)
          case _ => sys.error("equal applied to incompatible arguments")
        }))
      }))
    }
  }

  def eval(a: Exp, e: Env, k: Cont): TailRec[Val] = a match {
    case IntConst(n) =>
      tailcall(k(IntVal(n)))
    case BoolConst(b) =>
      tailcall(k(BoolVal(b)))
    case aa @ Var(name) =>
      tailcall(k(e(aa)))
    case Appl(opr, opnd) =>
      tailcall(eval(opr, e, Cont { f => eval(opnd, e, Cont { f.asInstanceOf[FVal](_, k) }) }))
    case aa @ Lambda(param, body) =>
      tailcall(k(evlambda(aa, e)))
    case Cond(premise, conclusion, alternative) =>
      tailcall(eval(premise, e, Cont {
        case BoolVal(true) => eval(conclusion, e, k)
        case BoolVal(false) => eval(alternative, e, k)
        case _ => sys.error("conditional with non-boolean premise")
      }))
    case LetRec(dvar, dexp, body) =>
      def e2: Env = Env {
        case x => if (x.name == dvar.name) evlambda(dexp, e2) else e(x)
      }
      tailcall(eval(body, e2, k))
    case Escp(escv, body) =>
      tailcall(eval(body, e.ext(escv, FVal { case (a, _) => k(a) }), k))
  }

  def evlambda(lam: Lambda, e: Env): Val = FVal {
    case (a, k) => eval(lam.body, e.ext(lam.param, a), k)
  }
}
