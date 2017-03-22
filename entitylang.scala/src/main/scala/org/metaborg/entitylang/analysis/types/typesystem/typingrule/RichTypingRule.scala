package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin
import org.metaborg.entitylang.util._

class RichTypingRule[TermType <: HasOrigin, TypeType](implicit val typeSystem: TypeSystem[TermType, TypeType]) {

  type Rule[T] = TypingRule[TermType, TypeType, T]
  type Result[T] = Rule[T]#Result

  def infer[Term <: TermType](term: Term): Rule[TypeType] = new InferTypingRule(term)

  def success[T](t: T): Rule[T] = result(Right(TypingResult(t, Map.empty)))
  def success[T <: TypeType](term: TermType, t: T): TermTypingRule[TermType, TypeType, T] = result[T](Right(TypingResult(t, Map(term -> t)))).bindTerm(term)
  def fail[T](term: HasOrigin, message: String): TermTypingRule[TermType, TypeType, T] = new FailTypingRule[TermType, TypeType, T](term, message)
  def mismatchedType[T <: TypeType](term: HasOrigin, expected: T, got: T): Rule[T] = new FailTypingRule[TermType, TypeType, T](term, s"expected type: ${typeSystem.prettyPrint(expected)}, got: ${typeSystem.prettyPrint(got)}")

  def matching(terms: TermType*): TypingRule[TermType, TypeType, TypeType] = new MatchingTypingRule[TermType, TypeType](terms:_*)

  def result[T](res: Result[T]): Rule[T] = new ResultTypingRule[TermType, TypeType, T](res)

  def fromTypeEnvironment(term: HasOrigin, name: String): TermTypingRule[TermType, TypeType, TypeType] = typeSystem.typeEnvironment.get(name) match {
    case Some(tpe) => success(tpe).bindTerm(term)
    case None => fail(term, s"Could not find field $name").bindTerm(term)
  }

  def alternative[T1 <: TypeType, T2 <: TypeType](r1: Rule[T1], r2: Rule[T2]): Rule[TypeType] = new AlternativeTypingRule(r1, r2)

  def all = new AllTypingRuleBuilder[TermType, TypeType]

  def all[T](rules: Seq[TypingRule[TermType, TypeType, T]]): TypingRule[TermType, TypeType, Seq[T]] = {
    val merged = EitherExtensions.merge(rules.map(_.run))
    val res = merged.right.map(t => TypingResult(t = t.map(_.t), subTypes = t.foldLeft(Map.empty[TermType, TypeType])(_ ++ _.subTypes)))
    result(res)
  }


  def withTypeSystem[TermType2 <: HasOrigin, TypeType2](term: TermType2, t: TypeSystem[TermType2, TypeType2]): Rule[TypeType2] = result(t.infer(term).right.map(_.copy(subTypes = Map.empty)))
}