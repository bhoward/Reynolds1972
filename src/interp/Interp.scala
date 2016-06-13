package interp

trait Interp {
  def apply(a: Exp): Val
}