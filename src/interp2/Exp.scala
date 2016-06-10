package interp2

trait Exp {
  def interpret: Val = eval(Init)
  
  def eval(e: Env): Val
}

case class IntConst(n: Int) extends Exp {
  def eval(e: Env): Val = IntVal(n)
}

case class BoolConst(b: Boolean) extends Exp {
  def eval(e: Env): Val = BoolVal(b)
}

case class Var(name: String) extends Exp {
  def eval(e: Env): Val = e.get(name)
}

case class Appl(opr: Exp, opnd: Exp) extends Exp {
  def eval(e: Env): Val = opr.eval(e)(opnd.eval(e))
}

case class Lambda(param: Var, body: Exp) extends Exp {
  def eval(e: Env): Val = Closure(this, e)
}

case class Cond(premise: Exp, conclusion: Exp, alternative: Exp) extends Exp {
  def eval(e: Env): Val = premise.eval(e) match {
    case BoolVal(true) => conclusion.eval(e)
    case BoolVal(false) => alternative.eval(e)
    case _ => sys.error("conditional with non-boolean premise")
  }
}

case class LetRec(dvar: Var, dexp: Lambda, body: Exp) extends Exp {
  def eval(e: Env): Val = body.eval(Rec(this, e))
}