package bhoward.interp4

import bhoward.interp._

import scala.util.control.TailCalls._

object Interp extends Interp {
  def apply(a: Exp): Val = eval(a, initEnv, Mem(), Cont { (_, v) => done(v) }).result

  val initEnv: Env = Env {
    case Var("succ") => FVal {
      case (v, m, k) => tailcall(k(m, v.succ))
    }
    case Var("pred") => FVal {
      case (v, m, k) => tailcall(k(m, v.pred))
    }
    case Var("ref") => FVal {
      case (v, m, k) => tailcall(k(m.augment(v), m.nextRef))
    }
    case Var("val") => FVal {
      case (r: Ref, m, k) => tailcall(k(m, m.lookup(r)))
      case _ => sys.error("val of a non-reference")
    }
    case Var("set") => FVal {
      case (r: Ref, m, k) => tailcall(k(m, FVal {
        case (v, m1, k) => tailcall(k(m1.update(r, v), v))
      }))
      case _ => sys.error("set of a non-reference")
    }
    case Var("=") => FVal {
      case (v, m, k) => tailcall(k(m, FVal {
        case (w, m1, k) => tailcall(k(m1, v equal w))
      }))
    }
    case Var("+") => FVal {
      case (v, m, k) => tailcall(k(m, FVal {
        case (w, m1, k) => tailcall(k(m1, v plus w))
      }))
    }
    case Var("-") => FVal {
      case (v, m, k) => tailcall(k(m, FVal {
        case (w, m1, k) => tailcall(k(m1, v minus w))
      }))
    }
    case Var("*") => FVal {
      case (v, m, k) => tailcall(k(m, FVal {
        case (w, m1, k) => tailcall(k(m1, v times w))
      }))
    }
    case Var("/") => FVal {
      case (v, m, k) => tailcall(k(m, FVal {
        case (w, m1, k) => tailcall(k(m1, v divide w))
      }))
    }
  }

  def eval(a: Exp, e: Env, m: Mem, k: Cont): TailRec[Val] = a match {
    case IntConst(n) =>
      tailcall(k(m, IntVal(n)))
    case BoolConst(b) =>
      tailcall(k(m, BoolVal(b)))
    case aa @ Var(name) =>
      tailcall(k(m, e(aa)))
    case Appl(opr, opnd) =>
      tailcall(eval(opr, e, m, Cont { (m1, f) =>
        eval(opnd, e, m1, Cont { (m2, v) =>
          f.asInstanceOf[FVal](v, m2, k)
        })
      }))
    case aa @ Lambda(param, body) =>
      tailcall(k(m, evlambda(aa, e)))
    case Cond(premise, conclusion, alternative) =>
      tailcall(eval(premise, e, m, Cont {
        case (m1, BoolVal(true)) => eval(conclusion, e, m1, k)
        case (m1, BoolVal(false)) => eval(alternative, e, m1, k)
        case _ => sys.error("conditional with non-boolean premise")
      }))
    case LetRec(dvar, dexp, body) =>
      def e2: Env = Env {
        case x => if (x.name == dvar.name) evlambda(dexp, e2) else e(x)
      }
      tailcall(eval(body, e2, m, k))
    case Escp(escv, body) =>
      tailcall(eval(body, e.ext(escv, FVal { case (a, m1, _) => k(m1, a) }), m, k))
  }

  def evlambda(lam: Lambda, e: Env): Val = FVal {
    case (a, m, k) => eval(lam.body, e.ext(lam.param, a), m, k)
  }
}
