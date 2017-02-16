package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.Type
import org.metaborg.entitylang.lang.ast.MExpression.SExp

package object typingrule {
  def numeric(e: SExp) = new NumericTypingRule(e)
}
