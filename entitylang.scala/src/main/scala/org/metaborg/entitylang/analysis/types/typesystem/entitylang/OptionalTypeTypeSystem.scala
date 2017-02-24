package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.lang.ast.MModel.SOptionalType
import org.metaborg.entitylang.lang.ast.MModel.SOptionalType.{DerivedType0, ExplicitType1}

object OptionalTypeTypeSystem extends SimpleTypeSystem[SOptionalType, Type]( implicit typeSystem => {
    case ExplicitType1(primitivetypewithmultiplicity1, origin) =>
      typeRule.result(PrimitiveTypeWithMultiplicityTypeSystem.infer(primitivetypewithmultiplicity1))
    case DerivedType0(origin) =>
      typeRule.success(top)
  }
)
