package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.{BaseType, EntityType, Type}
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.TypingResult
import org.metaborg.entitylang.lang.ast.MType.SType.{EntityType1, PrimitiveType1}
import org.metaborg.entitylang.lang.ast.MType.{SPrimitiveType, SType}

object BaseTypeTypeSystem extends SimpleTypeSystem[SType, BaseType](implicit typeSystem => {
  case PrimitiveType1(t, _) => typeRule.withTypeSystem(t, PrimitiveTypeTypeSystem)
  case EntityType1(name, o) => typeRule.success(EntityType(name.string))
})