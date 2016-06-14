package bhoward.interp4

import bhoward.interp._

import scala.util.control.TailCalls._

case class Cont(fun: Val => TailRec[Val]) {
  def apply(a: Val): TailRec[Val] = tailcall(fun(a))
}
