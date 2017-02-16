package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem

import scala.reflect.{ClassTag, classTag}

class OfTypeTypingRule[TermType0, TypeType0, T0 <: TypeType0: ClassTag](e: TermType0, humanReadableName: Option[String] = None)(implicit typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  val cls = classTag[T0].runtimeClass.asInstanceOf[Class[T]]

  override def run(implicit typeSystem: TypeSystemT): TypingResult = {
    typeSystem.infer(e).right.flatMap(t => if(cls.isInstance(t)) Right(cls.cast(t)) else typeError(e, s"Expected type: ${humanReadableName.getOrElse(cls.getTypeName)}, got: $t"))
  }
}
