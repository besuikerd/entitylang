package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

import scala.reflect.{ClassTag, classTag}

class OfTypeTypingRule[TermType <: HasOrigin, TypeType, T <: TypeType: ClassTag](val term: TermType, t: T, humanReadableName: Option[String] = None)(implicit typeSystem: TypeSystem[TermType, TypeType]) extends TermTypingRule[TermType, TypeType, T]{
  val cls = classTag[T].runtimeClass.asInstanceOf[Class[T]]

  override def run(implicit typeSystem: TypeSystemT): Result = {
    typeSystem.infer(term).right.flatMap(t => if(cls.isInstance(t.t)) Right(t.copy(t = cls.cast(t))) else typeError(term, s"Expected type: ${humanReadableName.getOrElse(cls.getTypeName)}, got: $t"))
  }
}
