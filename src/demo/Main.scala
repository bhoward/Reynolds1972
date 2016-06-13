package demo

object Main extends App {
  import interp._

  def Let(v: Var, x: Exp, body: Exp): Exp = Appl(Lambda(v, body), x)

  def Eq(a: Exp, b: Exp): Exp = Appl(Appl(Var("equal"), a), b)

  def Succ(a: Exp): Exp = Appl(Var("succ"), a)

  def pred(body: Exp): Exp = Let(Var("pred"),
    Lambda(Var("n"),
      LetRec(Var("aux"),
        Lambda(Var("x"),
          Cond(Eq(Succ(Var("x")), Var("n")),
            Var("x"),
            Appl(Var("aux"), Succ(Var("x"))))),
        Cond(Eq(Var("n"), IntConst(0)),
          IntConst(0),
          Appl(Var("aux"), IntConst(0))))),
    body)

  // given base: Int and ind: Int => Int => Int,
  // produces f: Int => Int such that
  // f(0) = base
  // f(n) = ind(n)(f(n-1))
  // Assumes pred already defined
  def PrimRec(base: Exp, ind: Exp): Exp = LetRec(Var("aux"),
    Lambda(Var("n"),
      Cond(Eq(Var("n"), IntConst(0)),
        base,
        Appl(Appl(ind, Var("n")), Appl(Var("aux"), Appl(Var("pred"), Var("n")))))),
    Var("aux"))

  def plus(body: Exp): Exp = pred(Let(Var("plus"),
    Lambda(Var("a"),
      PrimRec(Var("a"),
        Lambda(Var("_"),
          Var("succ")))),
    body))

  def times(body: Exp): Exp = plus(Let(Var("times"),
    Lambda(Var("a"),
      PrimRec(IntConst(0),
        Lambda(Var("_"),
          Appl(Var("plus"), Var("a"))))),
    body))

  def fib(body: Exp): Exp = times(Let(Var("fib"),
    PrimRec(IntConst(1), Var("times")),
    body))

  val test = fib(Appl(Var("fib"), IntConst(5)))

  def run(i: Interp): Unit = {
    println(i(test).show)

    val start = System.currentTimeMillis
    for (_ <- 1 to 100) i(test)
    val end = System.currentTimeMillis
    println((end - start) / 100 + " msec")
  }
  
  println("Interpreter 1 -- original")
  run(interp1.Interp)
  
  println("Interpreter 2 -- no HO functions")
  run(interp2.Interp)
  
  println("Interpreter 3 -- continuations and no HO functions")
  run(interp3.Interp)
  
  println("Interpreter 3a -- same as 3 but tail-recursive instead of trampolined")
  run(interp3a.Interp)
  
  println("Interpreter 4 -- continuations and HO functions (trampolined)")
  run(interp4.Interp)
}