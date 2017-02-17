package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem._

import scala.reflect.{ClassTag, classTag}

trait TermTypingRule[TermType0, TypeType0, T0 <: TypeType0] extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  val term: TermType0

  override def flatMap[U <: TypeType0](f: (T0) => Rule[U])(implicit typeSystem: TypeSystemT): Rule[U] = super.flatMap(f)

  def ofType[U <: T : ClassTag](implicit typeSystem: TypeSystemT): TermTypingRule[TermType0, TypeType0, U] = ofType(None)
  def ofType[U <: T : ClassTag](humanReadableName: String)(implicit typeSystem: TypeSystemT): TermTypingRule[TermType0, TypeType0, U] = ofType(Some(humanReadableName))
  def ofType[U <: T : ClassTag](humanReadableName: Option[String])(implicit typeSystem: TypeSystemT): TermTypingRule[TermType0, TypeType0, U] = {
    val x = flatMap{ t =>
      val cls = classTag[U].runtimeClass.asInstanceOf[Class[U]]
      if(cls.isInstance(t))
        rule.success[U](cls.cast(t))
      else
        rule.fail[U](term, s"Expected type: ${humanReadableName.getOrElse(cls.getTypeName)}, got: $t")
    }
    x.bindTerm(term)
  }
}