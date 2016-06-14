package bhoward.interp

sealed trait Exp

case class IntConst(n: Int) extends Exp

case class BoolConst(b: Boolean) extends Exp

case class Var(name: String) extends Exp

case class Appl(opr: Exp, opnd: Exp) extends Exp

case class Lambda(param: Var, body: Exp) extends Exp

case class Cond(premise: Exp, conclusion: Exp, alternative: Exp) extends Exp

case class LetRec(dvar: Var, dexp: Lambda, body: Exp) extends Exp

case class Escp(escv: Var, body: Exp) extends Exp
