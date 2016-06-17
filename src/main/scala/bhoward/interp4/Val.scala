package bhoward.interp4

import bhoward.interp._

import scala.util.control.TailCalls._

case class FVal(fun: (Val, Mem, Cont) => TailRec[Val]) extends FunVal {
  def apply(x: Val, m: Mem, k: Cont): TailRec[Val] = tailcall(fun(x, m, k))
}
