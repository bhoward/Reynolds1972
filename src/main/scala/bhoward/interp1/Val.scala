package bhoward.interp1

import bhoward.interp._

case class FVal(fun: Val => Val) extends FunVal {
  def apply(x: Val): Val = fun(x)
}
