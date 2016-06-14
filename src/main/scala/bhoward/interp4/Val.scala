package bhoward.interp4

import bhoward.interp._

import scala.util.control.TailCalls._

case class FVal(fun: (Val, Cont) => TailRec[Val]) extends FunVal {
  def apply(x: Val, k: Cont): TailRec[Val] = tailcall(fun(x, k))
}
