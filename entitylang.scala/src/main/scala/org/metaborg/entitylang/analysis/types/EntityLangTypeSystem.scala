package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.analysis.types.typesystem.typingrule.SuccessTypingRule
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.{False0, Int1, True0}
import typesystem._

object EntityLangTypeSystem {
  implicit val typeSystem = TypeSystem(
    if3,
    true0,
    false0,
    int1
  )

  def if3: TopLevelTypingRule[SExp, Type] = {
    case If3(e1, e2, e3, _) =>
      for{
        t1 <- e1.ofType[BooleanType]
        t2 <- matching(e2, e3)
        t3 <- e2.fail("Nope")
      } yield t2
  }

  def true0: TopLevelTypingRule[SExp, Type] = {
    case t @ True0(_) => success(boolean)
  }

  def false0: TopLevelTypingRule[SExp, Type] = {
    case False0(_) => success(boolean)
  }

  def int1: TopLevelTypingRule[SExp, Type] = {
    case Int1(_, _) => success(int)
  }
}
