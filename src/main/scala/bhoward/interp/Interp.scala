package bhoward.interp

trait Interp {
  def apply(a: Exp): Val
}
