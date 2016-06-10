package interp1

trait Exp {
  def interpret: Val = eval(Exp.initEnv)

  def eval(e: Env): Val
}

object Exp {
  val initEnv: Env = Env {
    case Var("succ") => FunVal {
      case IntVal(n) => IntVal(n + 1)
      case _ => sys.error("succ applied to non-integral argument")
    }
    case Var("equal") => FunVal {
      case a => FunVal {
        case b => (a, b) match {
          case (IntVal(n1), IntVal(n2)) => BoolVal(n1 == n2)
          case (BoolVal(b1), BoolVal(b2)) => BoolVal(b1 == b2)
          case _ => sys.error("equal applied to incompatible arguments")
        }
      }
    }
  }
}

case class IntConst(n: Int) extends Exp {
  def eval(e: Env): Val = IntVal(n)
}

case class BoolConst(b: Boolean) extends Exp {
  def eval(e: Env): Val = BoolVal(b)
}

case class Var(name: String) extends Exp {
  def eval(e: Env): Val = e(this)
}

case class Appl(opr: Exp, opnd: Exp) extends Exp {
  def eval(e: Env): Val = opr.eval(e)(opnd.eval(e))
}

case class Lambda(param: Var, body: Exp) extends Exp {
  def eval(e: Env): Val = FunVal {
    case a => body.eval(e.ext(param, a))
  }
}

case class Cond(premise: Exp, conclusion: Exp, alternative: Exp) extends Exp {
  def eval(e: Env): Val = premise.eval(e) match {
    case BoolVal(true) => conclusion.eval(e)
    case BoolVal(false) => alternative.eval(e)
    case _ => sys.error("conditional with non-boolean premise")
  }
}

case class LetRec(dvar: Var, dexp: Lambda, body: Exp) extends Exp {
  def eval(e: Env): Val = {
    def e2: Env = Env {
      case x => if (x.name == dvar.name) dexp.eval(e2) else e(x)
    }
    body.eval(e2)
  }
}
