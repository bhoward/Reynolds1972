package bhoward.interp1

import bhoward.interp._

object Interp extends Interp {
  def apply(a: Exp): Val = eval(a, initEnv)

  val initEnv: Env = Env {
    case Var("succ") => FVal { _.succ }
    case Var("equal") => FVal { v => FVal { v equal _ } }
  }

  def eval(a: Exp, e: Env): Val = a match {
    case IntConst(n) =>
      IntVal(n)
    case BoolConst(b) =>
      BoolVal(b)
    case aa @ Var(name) =>
      e(aa)
    case Appl(opr, opnd) =>
      eval(opr, e).asInstanceOf[FVal](eval(opnd, e))
    case Lambda(param, body) =>
      FVal { v => eval(body, e.ext(param, v)) }
    case Cond(premise, conclusion, alternative) =>
      eval(premise, e) match {
        case BoolVal(true) => eval(conclusion, e)
        case BoolVal(false) => eval(alternative, e)
        case _ => sys.error("conditional with non-boolean premise")
      }
    case LetRec(dvar, dexp, body) =>
      def e2: Env = Env {
        case x => if (x.name == dvar.name) eval(dexp, e2) else e(x)
      }
      eval(body, e2)
    case Escp(escv, body) =>
      sys.error("escape operation not available")
  }
}
