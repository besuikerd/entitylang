package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, TypeError}

class FromTypeEnvironmentTypingRule[TermType0, TypeType0](term: TermType0, name: String) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = TypeType0

  override def run(implicit typeSystem: TypeSystemT): TypingResult =
    typeSystem.typeEnvironment.get(name).toRight(GeneralTypeError(typeSystem.getOrigin(term), "Could not find type for identifier " + name))
}
