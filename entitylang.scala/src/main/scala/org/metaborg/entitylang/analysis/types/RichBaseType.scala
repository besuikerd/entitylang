package org.metaborg.entitylang.analysis.types

import multiplicity._

class RichBaseType[T <: BaseType](val t: T) extends AnyVal {
  def *(): MultiplicityType[T] = MultiplicityType(t, zeroToMany)

  def ?(): MultiplicityType[T] = MultiplicityType(t, zeroToOne)

  def +(): MultiplicityType[T] = MultiplicityType(t, oneToMany)

  def one: MultiplicityType[T] = MultiplicityType(t, oneToOne)

  def zero: MultiplicityType[T] = MultiplicityType(t, zeroToZero)

  def ~>:(t2: BaseType): FunctionType = FunctionType(t2.one, t.one)
  def ~>:(t2: Type): FunctionType = FunctionType(t2, t.one)
}