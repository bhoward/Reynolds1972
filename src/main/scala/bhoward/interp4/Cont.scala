package bhoward.interp4

import bhoward.interp._

import scala.util.control.TailCalls._

case class Cont(fun: (Mem, Val) => TailRec[Val]) {
  def apply(m: Mem, a: Val): TailRec[Val] = tailcall(fun(m, a))
}
