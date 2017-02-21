package org.metaborg.entitylang.analysis

package object types {
  @inline implicit def richType(t: Type): RichType = new RichType(t)
  @inline implicit def richBaseType[T <: BaseType](t: T): RichBaseType[T] = new RichBaseType(t)

  implicit val numericOrdering: Ordering[NumericType] = new Ordering[NumericType]{
    override def compare(x: NumericType, y: NumericType): Int = orderNum(x) - orderNum(y)

    def orderNum(t: NumericType) = t match {
      case IntType() => 0
      case LongType() => 1
      case FloatType() => 2
    }
  }

  implicit def multiplicityTypeOrdering[T <: BaseType](implicit ord: Ordering[T]): Ordering[MultiplicityType[T]] = new Ordering[MultiplicityType[T]]{
    override def compare(x: MultiplicityType[T], y: MultiplicityType[T]): Int = ord.compare(x.baseType, y.baseType)
  }

  val top = TopType()
  val long = LongType()
  val int = IntType()
  val float = FloatType()
  val string = StringType()
  val boolean = BooleanType()

}
