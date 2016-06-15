package bhoward.interp3a

import bhoward.interp._

case class Closure(lam: Lambda, en: Env) extends FunVal

case object Succ extends FunVal

case object Pred extends FunVal

case object Eq1 extends FunVal

case class Eq2(arg: Val) extends FunVal

case object Plus1 extends FunVal

case class Plus2(arg: Val) extends FunVal

case object Minus1 extends FunVal

case class Minus2(arg: Val) extends FunVal

case object Times1 extends FunVal

case class Times2(arg: Val) extends FunVal

case object Divide1 extends FunVal

case class Divide2(arg: Val) extends FunVal

case class Escf(cn: Cont) extends FunVal
