package bhoward.interp4

import bhoward.interp._

import scala.util.control.TailCalls._

object Interp extends Interp {
  def apply(a: Exp): Val = eval(a, initEnv, Cont { done(_) }).result

  val initEnv: Env = Env {
    case Var("succ") => FVal {
      case (v, k) => tailcall(k(v.succ))
    }
    case Var("pred") => FVal {
      case (v, k) => tailcall(k(v.pred))
    }
    case Var("=") => FVal {
      case (v, k) => tailcall(k(FVal {
        case (w, k) => tailcall(k(v equal w))
      }))
    }
    case Var("+") => FVal {
      case (v, k) => tailcall(k(FVal {
        case (w, k) => tailcall(k(v plus w))
      }))
    }
    case Var("-") => FVal {
      case (v, k) => tailcall(k(FVal {
        case (w, k) => tailcall(k(v minus w))
      }))
    }
    case Var("*") => FVal {
      case (v, k) => tailcall(k(FVal {
        case (w, k) => tailcall(k(v times w))
      }))
    }
    case Var("/") => FVal {
      case (v, k) => tailcall(k(FVal {
        case (w, k) => tailcall(k(v divide w))
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
