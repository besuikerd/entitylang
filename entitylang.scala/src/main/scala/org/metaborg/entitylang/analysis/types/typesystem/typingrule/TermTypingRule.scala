package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.scalaterms.HasOrigin

import scala.reflect.{ClassTag, classTag}

trait TermTypingRule[TermType0 <: HasOrigin, TypeType0, T0 <: TypeType0] extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  val term: TermType0

  override def flatMap[U <: TypeType0](f: (T0) => Rule[U])(implicit typeSystem: TypeSystemT): Rule[U] = super.flatMap(f)


  def fail(message: String)(implicit typeSystem: TypeSystemT): TermTypingRule[TermType0, TypeType0, T] =
    rule.fail(term, message).bindTerm(term)

  def ofType[U <: T : ClassTag](implicit typeSystem: TypeSystemT): TermTypingRule[TermType0, TypeType0, U] = ofType(None)
  def ofType[U <: T : ClassTag](humanReadableName: String)(implicit typeSystem: TypeSystemT): TermTypingRule[TermType0, TypeType0, U] = ofType(Some(humanReadableName))
  def ofType[U <: T : ClassTag](humanReadableName: Option[String])(implicit typeSystem: TypeSystemT): TermTypingRule[TermType0, TypeType0, U] = {
    val r = flatMap{ t =>
      val cls = classTag[U].runtimeClass.asInstanceOf[Class[U]]
      if(cls.isInstance(t))
        rule.success[U](cls.cast(t))
      else
        rule.fail[U](term, s"Expected type: ${humanReadableName.getOrElse(cls.getTypeName)}, got: $t")
    }
    r.bindTerm(term)
  }

  def ofType[U <: T](t: U)(implicit typeSystemT: TypeSystemT): TermTypingRule[TermType0, TypeType0, U] = {
    val r = flatMap[U]{ t2 =>
      if (t == t2) {
        rule.success[U](t)
      } else {
        rule.fail[U](term, s"Expected type: $t, got: $t2")
      }
    }
    r.bindTerm(term)
  }
}