package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.multiplicity._
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{TypingResult, TypingRule}
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveTypeWithMultiplicity
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveTypeWithMultiplicity.{PrimitiveTypeWithDefaultMultiplicity1, PrimitiveTypeWithMultiplicity2}

object PrimitiveTypeWithMultiplicityTypeSystem extends SimpleTypeSystem[SPrimitiveTypeWithMultiplicity, Type]( implicit typeSystem => {
  case PrimitiveTypeWithMultiplicity2(primitivetype1, multiplicity2, origin) =>
    for {
      baseType <- typeRule.withTypeSystem(primitivetype1, PrimitiveTypeTypeSystem)
      multiplicity <- typeRule.withTypeSystem(multiplicity2, MultiplicityTypeSystem)
    } yield MultiplicityType(baseType, multiplicity)
  case PrimitiveTypeWithDefaultMultiplicity1(primitivetype1, origin) =>
    typeRule.withTypeSystem(primitivetype1, PrimitiveTypeTypeSystem).map(_.one)
})
