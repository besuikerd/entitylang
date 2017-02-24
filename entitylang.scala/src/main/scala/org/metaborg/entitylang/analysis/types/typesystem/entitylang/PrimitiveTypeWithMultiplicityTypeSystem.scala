package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.multiplicity._
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveTypeWithMultiplicity
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveTypeWithMultiplicity.{PrimitiveTypeWithDefaultMultiplicity1, PrimitiveTypeWithMultiplicity2}

object PrimitiveTypeWithMultiplicityTypeSystem extends SimpleTypeSystem[SPrimitiveTypeWithMultiplicity, Type]( implicit typeSystem => {

  case PrimitiveTypeWithMultiplicity2(primitivetype1, multiplicity2, origin) =>
    val result = for {
      baseType <- BaseTypeTypeSystem.infer(primitivetype1).right
      multiplicity <- MultiplicityTypeSystem.infer(multiplicity2).right
    } yield MultiplicityType(baseType, multiplicity)
    typeRule.result(result)
  case PrimitiveTypeWithDefaultMultiplicity1(primitivetype1, origin) =>
    val result = BaseTypeTypeSystem.infer(primitivetype1).right.map(t => MultiplicityType(t, oneToOne))
    typeRule.result(result)
})
