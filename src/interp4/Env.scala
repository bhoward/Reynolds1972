package interp4

case class Env(fun: Var => Val) {
  def apply(x: Var): Val = fun(x)

  def ext(z: Var, a: Val): Env = Env {
    case x => if (x.name == z.name) a else this(x)
  }
}
