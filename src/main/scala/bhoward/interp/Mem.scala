package bhoward.interp

case class Mem(count: Int = 0, possess: Int => Val = (_ => IntVal(0))) {
  def nextRef: Ref = Ref(count + 1)
  
  def augment(v: Val): Mem = Mem(count + 1, n => if (n == count + 1) v else possess(n))
  
  def update(r: Ref, v: Val): Mem = Mem(count, n => if (n == r.num) v else possess(n))
  
  def lookup(r: Ref): Val = possess(r.num)
}
