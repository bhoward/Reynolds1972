package bhoward.demo

object Parser {
  import fastparse.WhitespaceApi

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" \t\n").rep)
  }

  import fastparse.noApi._
  import White._

  import bhoward.interp._

  val name = P(CharPred(Character.isJavaIdentifierStart(_)) ~~
    CharPred(Character.isJavaIdentifierPart(_)).repX).!.opaque("identifier")

  val ident = P[Var](name.map(Var(_)))

  val number = P(CharIn('0' to '9').repX(1)).!.map(_.toInt).opaque("number")

  val bool = P(StringIn("true", "false").!.map(_.toBoolean)).opaque("boolean")

  val const = P[Exp](number.map(IntConst(_)) | bool.map(BoolConst(_))).opaque("constant")

  def combine(first: Exp, rest: Seq[(String, Exp)]): Exp = rest.foldLeft(first) {
    case (a, (op, b)) => Appl(Appl(Var(op), a), b)
  }

  val relexp = P[Exp]((addexp ~ (StringIn("=").! ~ addexp).rep).map {
    case (first, rest) => combine(first, rest)
  })

  val addexp = P[Exp]((mulexp ~ (StringIn("+", "-").! ~ mulexp).rep).map {
    case (first, rest) => combine(first, rest)
  })

  val mulexp = P[Exp]((app ~ (StringIn("*", "/").! ~ app).rep).map {
    case (first, rest) => combine(first, rest)
  })
  
  val app = P[Exp]((factor ~ ("(" ~/ exp ~ ")").rep).map {
    case (a, bs) => bs.foldLeft(a)(Appl(_, _))
  })

  val factor = P[Exp](const | ident | "(" ~/ exp ~ ")")

  val cond = P[Exp](("if" ~/ exp ~ "then" ~ exp ~ "else" ~ exp).map {
    case (premise, conclusion, alternative) => Cond(premise, conclusion, alternative)
  })

  val fun = P[Exp](("fun" ~/ ident ~ "=>" ~ exp).map {
    case (param, body) => Lambda(param, body)
  })

  val valdecl = P[Exp => Exp](("val" ~/ ident ~ "=" ~ exp).map {
    case (dvar, dexp) => body => Appl(Lambda(dvar, body), dexp)
  })

  val defdecl = P[Exp => Exp](("def" ~/ ident ~ "(" ~ ident ~ ")" ~ "=" ~ exp).map {
    case (dvar, dparam, dbody) => body => LetRec(dvar, Lambda(dparam, dbody), body)
  })
  
  val escdecl = P[Exp => Exp](("esc" ~/ ident).map {
    case escv => body => Escp(escv, body)
  })

  val decls = P[Exp => Exp]((valdecl | defdecl).rep.map {
    case ds => body => ds.foldRight(body)(_ apply _)
  })

  val let = P[Exp](("let" ~/ decls ~ "in" ~ exp).map {
    case (dfun, body) => dfun(body)
  })

  val exp: Parser[Exp] = P(cond | fun | let | relexp)
  
  def apply(in: String): Exp = (exp ~ End).parse(in).get.value
}