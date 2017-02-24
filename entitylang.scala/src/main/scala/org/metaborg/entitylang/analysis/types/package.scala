package org.metaborg.entitylang.analysis

package object types {
  @inline implicit def richType(t: Type): RichType = new RichType(t)
  @inline implicit def richBaseType[T <: BaseType](t: T): RichBaseType[T] = new RichBaseType(t)

  val top = TopType()
  val long = LongType()
  val int = IntType()
  val float = FloatType()
  val string = StringType()
  val boolean = BooleanType()
  val any = AnyType()

}
