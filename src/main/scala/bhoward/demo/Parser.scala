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

  val valdecl = P[Exp](("val" ~/ ident ~ "=" ~ exp ~ "in" ~ exp).map {
    case (dvar, dexp, body) => Appl(Lambda(dvar, body), dexp)
  })

  // TODO allow "def f1(x1) = b1 and f2(x2) = b2 and ... in body" -- mutual recursion;
  // translate to "letrec foo = S<fun x1 => b1, fun x2 => b2, ...> in S(body)" where
  // S is substitute foo.i for fi and foo is new;
  // define <a1, a2, ...> = fun p => p(a1)(a2)..., so a.i is appl. to correct projection
  // (but special-case single-recursion)
  val defdecl = P((ident ~ "(" ~ ident ~ ")" ~ "=" ~ exp).map {
    case (dvar, param, body) => (dvar, Lambda(param, body))
  })

  var seqNum: Int = 0

  def gensym: Var = {
    seqNum += 1
    Var("_" + seqNum)
  }

  def proj(i: Int, n: Int): Exp = {
    val xs = (0 until n) map {_ => gensym}
    xs.foldRight[Exp](xs(i)){
      case (x, r) => Lambda(x, r)
    }
  }

  def tuple(as: Seq[Exp]): Exp = {
    val p = gensym
    Lambda(p, as.foldLeft[Exp](p)(Appl(_, _)))
  }

  def subst(a: Exp, dsubs: Map[String, Exp]): Exp = a match {
    case Var(name) =>
      dsubs.getOrElse(name, a)
    case Appl(opr, opnd) =>
      Appl(subst(opr, dsubs), subst(opnd, dsubs))
    case Lambda(param, body) =>
      Lambda(param, subst(body, dsubs - param.name))
    case Cond(premise, conclusion, alternative) =>
      Cond(subst(premise, dsubs), subst(conclusion, dsubs), subst(alternative, dsubs))
    case LetRec(dvar, dexp, body) =>
      LetRec(dvar, subst(dexp, dsubs - dvar.name).asInstanceOf[Lambda], subst(body, dsubs - dvar.name))
    case Escp(escv, body) =>
      Escp(escv, subst(body, dsubs - escv.name))
    case _ => a
  }

  val defdecls = P[Exp](("def" ~/ defdecl.rep(1, sep = "and") ~ "in" ~ exp).map {
    case (decls, body) =>
      decls.toList match {
        case (dvar, dexp) :: Nil => LetRec(dvar, dexp, body)
        case _ =>
          val n = decls.size
          val dvars = decls map { _._1 }
          val dexps = decls map { _._2 }
          val cvar = gensym
          val dsubs = (dvars.zipWithIndex map { case (v, i) => (v.name, Appl(cvar, proj(i, n))) }).toMap
          val cexp = subst(tuple(dexps), dsubs).asInstanceOf[Lambda]
          val cbody = subst(body, dsubs)
          LetRec(cvar, cexp, cbody)
      }
  })

  val escdecl = P[Exp](("esc" ~/ ident ~ "in" ~ exp).map {
    case (escv, body) => Escp(escv, body)
  })

  val exp: Parser[Exp] = P(cond | fun | valdecl | defdecls | escdecl | relexp)

  def apply(in: String): Exp = (exp ~ End).parse(in).get.value
}