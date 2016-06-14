package bhoward.interp2

import bhoward.interp._

case class Closure(lam: Lambda, en: Env) extends FunVal

case object Succ extends FunVal

case object Eq1 extends FunVal

case class Eq2(arg: Val) extends FunVal
