package org.metaborg.entitylang.analysis.types.typesystem

import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, TypeError}
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.TypingRule.Aux
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{TermTypingRule, TypingRule}
import org.metaborg.scalaterms.{HasOrigin, Origin}

import scala.reflect.{ClassTag, classTag}


trait TypeSystem[TermType, TypeType]{
  def infer(ast: TermType): TypingRule.Aux[TermType, TypeType, TypeType]#TypingResult
  def typeEnvironment: Map[String, TypeType]
  def prettyPrint(t: TypeType): String = t.toString

  def withBinding(name: String, t: TypeType): TypeSystem[TermType, TypeType]
  def withBindings(bindings: Map[String, TypeType]): TypeSystem[TermType, TypeType] =
    bindings.foldLeft(this){
      case (typeSystem, (name, t)) => typeSystem.withBinding(name, t)
    }
}

trait FancyTypeSystem[TermType <: HasOrigin, TypeType] extends TypeSystem[TermType, TypeType]{ self =>
  override val typeEnvironment: Map[String, TypeType] = Map.empty

  type Rule[T] = TypeSystem[TermType, TypeType] => T => TypingRule.Aux[TermType, TypeType, TypeType]

  def rules: Seq[TopLevelTypingRule[TermType, TypeType]] = materializedRules

  private lazy val materializedRules = mutableRules.toSeq
  private val mutableRules: scala.collection.mutable.ListBuffer[TopLevelTypingRule[TermType, TypeType]] = scala.collection.mutable.ListBuffer.empty

  override def infer(ast: TermType): Aux[TermType, TypeType, TypeType]#TypingResult =
    rules
      .view
      .flatMap(rule => rule(this).andThen(x => Seq(x.run(this))).applyOrElse(ast, (_: TermType) => Seq.empty))
      .headOption.getOrElse(fail(ast, "Could not find valid typing rule to apply for term " + ast)(this).run(this))


  override def withBinding(name: String, t: TypeType): TypeSystem[TermType, TypeType] = new FancyTypeSystem[TermType, TypeType] {
    override val rules: Seq[TopLevelTypingRule[TermType, TypeType]] = self.rules
    override val typeEnvironment: Map[String, TypeType] = self.typeEnvironment + (name -> t)
  }

  override def withBindings(bindings: Map[String, TypeType]): TypeSystem[TermType, TypeType] = new FancyTypeSystem[TermType, TypeType] {
    override val rules: Seq[TopLevelTypingRule[TermType, TypeType]] = self.rules
    override val typeEnvironment: Map[String, TypeType] = self.typeEnvironment ++ bindings
  }

  protected def rule[T <: TermType : ClassTag](r: Rule[T]): Unit = {
    val cls = classTag[T].runtimeClass.asInstanceOf[Class[T]]
    val r2: TopLevelTypingRule[TermType, TypeType] = implicit typeSystem => {case x if cls.isInstance(x) => r(typeSystem)(cls.cast(x))}
    self.mutableRules += r2
  }

  protected def partialRule(r: TopLevelTypingRule[TermType, TypeType]): Unit = {
    self.mutableRules += r
  }
}


class TypeSystemImpl[TermType <: HasOrigin, TypeType](val rules: Seq[TopLevelTypingRule[TermType, TypeType]], val typeEnvironment: Map[String, TypeType], val prettyPrinter: TypeType => String) extends TypeSystem[TermType, TypeType]{
  override def infer(ast: TermType): TypingRule.Aux[TermType, TypeType, TypeType]#TypingResult =
    rules
      .view
      .flatMap(pf => pf(this).andThen(x => Seq(x.run(this))).applyOrElse(ast, (_: TermType) => Seq.empty))
      .headOption.getOrElse(fail(ast, "Could not find valid typing rule to apply for term " + ast)(this).run(this))

  override def withBinding(name: String, t: TypeType) = new TypeSystemImpl[TermType, TypeType](rules, typeEnvironment + (name -> t), prettyPrinter)
  override def withBindings(bindings: Map[String, TypeType]): TypeSystem[TermType, TypeType] = new TypeSystemImpl(rules, typeEnvironment ++ bindings, prettyPrinter)
}

object TypeSystem{
  def apply[TermType <: HasOrigin, TypeType](typeEnvironment: Map[String, TypeType], prettyPrinter: TypeType => String)(rules: TopLevelTypingRule[TermType, TypeType]*): TypeSystem[TermType, TypeType] = new TypeSystemImpl(rules, Map.empty, prettyPrinter)
  def apply[TermType <: HasOrigin, TypeType](rules: TopLevelTypingRule[TermType, TypeType]*): TypeSystem[TermType, TypeType] = apply(Map.empty[String, TypeType], (t: TypeType) => t.toString)(rules:_*)
}