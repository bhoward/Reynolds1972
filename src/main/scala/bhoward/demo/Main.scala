package bhoward.demo

object Main extends App {
  import bhoward.interp._

  val test = Parser("def fib(n) = if n = 0 then 0 else if n = 1 then 1 else fib(n - 1) + fib(n - 2) in fib(20)")

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
  run(bhoward.interp3b.Interp)
  
  println("Interpreter 4 -- continuations and HO functions (trampolined)")
  run(bhoward.interp4.Interp)
  
  val test2 = Parser("6 * (esc e in def f(n) = if n = 1 then e(7) else if n / 2 * 2 = n then f(n / 2) else f(3*n + 1) in f(27))")
  println(bhoward.interp3.Interp(test2).show)
  println(bhoward.interp3a.Interp(test2).show)
  println(bhoward.interp3b.Interp(test2).show)
  println(bhoward.interp4.Interp(test2).show)
  
  val test3 = Parser("def a(n) = if n = 0 then 42 else b(n - 1) and b(n) = if n = 0 then 37 else a(n - 1) in a(17)")
  println(bhoward.interp1.Interp(test3).show)
  println(bhoward.interp2.Interp(test3).show)
  println(bhoward.interp3.Interp(test3).show)
  println(bhoward.interp3a.Interp(test3).show)
  println(bhoward.interp3b.Interp(test3).show)
  println(bhoward.interp4.Interp(test3).show)
  
  val test4 = Parser("var x = 1 in begin let x := @x + 1; @x * @x end")
  println(bhoward.interp4.Interp(test4).show)
}

// TODO add assignments
