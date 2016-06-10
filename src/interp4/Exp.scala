package interp4

import scala.util.control.TailCalls._

trait Exp {
  def interpret: Val = eval(Exp.initEnv, Cont { a => done(a) }).result

  def eval(e: Env, k: Cont): TailRec[Val]
}

object Exp {
  val initEnv: Env = Env {
    case Var("succ") => FunVal {
      case (IntVal(n), k) => tailcall(k(IntVal(n + 1)))
      case _ => sys.error("succ applied to non-integral argument")
    }
    case Var("equal") => FunVal {
      case (a, k) => tailcall(k(FunVal {
        case (b, k) => tailcall(k((a, b) match {
          case (IntVal(n1), IntVal(n2)) => BoolVal(n1 == n2)
          case (BoolVal(b1), BoolVal(b2)) => BoolVal(b1 == b2)
          case _ => sys.error("equal applied to incompatible arguments")
        }))
      }))
    }
  }
}

case class IntConst(n: Int) extends Exp {
  override def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(IntVal(n)))
}

case class BoolConst(b: Boolean) extends Exp {
  override def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(BoolVal(b)))
}

case class Var(name: String) extends Exp {
  override def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(e(this)))
}

case class Appl(opr: Exp, opnd: Exp) extends Exp {
  override def eval(e: Env, k: Cont): TailRec[Val] = tailcall(opr.eval(e, Cont { f => opnd.eval(e, Cont { a => f(a, k) }) }))
}

case class Lambda(param: Var, body: Exp) extends Exp {
  override def eval(e: Env, k: Cont): TailRec[Val] = tailcall(k(evlambda(e)))

  def evlambda(e: Env): Val = FunVal {
    case (a, k) => body.eval(e.ext(param, a), k)
  }
}

case class Cond(premise: Exp, conclusion: Exp, alternative: Exp) extends Exp {
  override def eval(e: Env, k: Cont): TailRec[Val] = tailcall(premise.eval(e, Cont { b =>
    b match {
      case BoolVal(true) => conclusion.eval(e, k)
      case BoolVal(false) => alternative.eval(e, k)
      case _ => sys.error("conditional with non-boolean premise")
    }
  }))
}

case class LetRec(dvar: Var, dexp: Lambda, body: Exp) extends Exp {
  override def eval(e: Env, k: Cont): TailRec[Val] = {
    def e2: Env = Env {
      case x => if (x.name == dvar.name) dexp.evlambda(e2) else e(x)
    }
    tailcall(body.eval(e2, k))
  }
}
