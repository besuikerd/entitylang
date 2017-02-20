package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

import scala.reflect.{ClassTag, classTag}

class OfTypeTypingRule[TermType0 <: HasOrigin, TypeType0, T0 <: TypeType0: ClassTag](val term: TermType0, t: T0, humanReadableName: Option[String] = None)(implicit typeSystem: TypeSystem[TermType0, TypeType0]) extends TermTypingRule[TermType0, TypeType0, T0]{
  val cls = classTag[T0].runtimeClass.asInstanceOf[Class[T]]

  override def run(implicit typeSystem: TypeSystemT): TypingResult = {
    typeSystem.infer(term).right.flatMap(t => if(cls.isInstance(t)) Right(cls.cast(t)) else typeError(term, s"Expected type: ${humanReadableName.getOrElse(cls.getTypeName)}, got: $t"))
  }
}
