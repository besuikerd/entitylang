package org.metaborg.entitylang.analysis.types.typesystem.entitylang.typingrule

import org.metaborg.entitylang.analysis.types.Type
import org.metaborg.entitylang.analysis.types.typesystem.TypingRule
import org.metaborg.entitylang.lang.ast.MExpression.SExp

trait EntityLangTypingRule extends TypingRule{
  override type TermType = SExp
  override type TypeType = Type
}