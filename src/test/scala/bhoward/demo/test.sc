package bhoward.demo

object test {
  Parser.name.parse("x42")                        //> res0: fastparse.core.Parsed[String] = Success(x42,3)
  Parser.name.parse("hello")                      //> res1: fastparse.core.Parsed[String] = Success(hello,5)
  Parser.name.parse("22skidoo")                   //> res2: fastparse.core.Parsed[String] = Failure(identifier:1:1 ..."22skidoo")
  
  Parser.number.parse("x42")                      //> res3: fastparse.core.Parsed[Int] = Failure(number:1:1 ..."x42")
  Parser.number.parse("22skidoo")                 //> res4: fastparse.core.Parsed[Int] = Success(22,2)
  Parser.number.parse("0123")                     //> res5: fastparse.core.Parsed[Int] = Success(123,4)
  
  Parser.bool.parse("true")                       //> res6: fastparse.core.Parsed[Boolean] = Success(true,4)
  Parser.bool.parse("FaLsE")                      //> res7: fastparse.core.Parsed[Boolean] = Failure(boolean:1:1 ..."FaLsE")
  Parser.bool.parse("hello")                      //> res8: fastparse.core.Parsed[Boolean] = Failure(boolean:1:1 ..."hello")
  
  Parser.const.parse("1234")                      //> res9: fastparse.core.Parsed[bhoward.interp.Exp] = Success(IntConst(1234),4)
  Parser.const.parse("false")                     //> res10: fastparse.core.Parsed[bhoward.interp.Exp] = Success(BoolConst(false),
                                                  //| 5)
  Parser.const.parse("true22")                    //> res11: fastparse.core.Parsed[bhoward.interp.Exp] = Success(BoolConst(true),4
                                                  //| )
  
  Parser("if a = 0 then 40 + 2 * 1 else fun joe => true")
                                                  //> res12: bhoward.interp.Exp = Cond(Appl(Appl(Var(=),Var(a)),IntConst(0)),Appl(
                                                  //| Appl(Var(+),IntConst(40)),Appl(Appl(Var(*),IntConst(2)),IntConst(1))),Lambda
                                                  //| (Var(joe),BoolConst(true)))
  Parser("let val a = 1 def b(c) = 2 in b(a)")    //> res13: bhoward.interp.Exp = Appl(Lambda(Var(a),LetRec(Var(b),Lambda(Var(c),I
                                                  //| ntConst(2)),Appl(Var(b),Var(a)))),IntConst(1))
}