package bhoward.demo

object Main extends App {
  import bhoward.interp._

  val test = Parser("let def fib(n) = if n = 0 then 0 else if n = 1 then 1 else fib(n - 1) + fib(n - 2) in fib(20)")

  def run(i: Interp): Unit = {
    println(i(test).show)

    val start = System.currentTimeMillis
    for (_ <- 1 to 100) i(test)
    val end = System.currentTimeMillis
    println((end - start) / 100 + " msec")
  }
  
  println("Interpreter 1 -- original")
  run(bhoward.interp1.Interp)
  
  println("Interpreter 2 -- no HO functions")
  run(bhoward.interp2.Interp)
  
  println("Interpreter 3 -- continuations and no HO functions")
  run(bhoward.interp3.Interp)
  
  println("Interpreter 3a -- same as 3 but tail-recursive instead of trampolined")
  run(bhoward.interp3a.Interp)
  
  println("Interpreter 3b -- same as 3a but uses enums and named/default params instead of Either")
  run(bhoward.interp3a.Interp)
  
  println("Interpreter 4 -- continuations and HO functions (trampolined)")
  run(bhoward.interp4.Interp)
}

// TODO add ops to interpreters, use parser, test escape functions, then add assignments
