package org.metaborg.entitylang.analysis.types

class RichType(val t: Type) extends AnyVal {
  def ~>(t2: Type): Type = FunctionType(t, t2)
  def ~>:(t2: Type): Type = t2 ~> t
  def ~>:(t2: BaseType): Type = t2.one ~>: t
}
