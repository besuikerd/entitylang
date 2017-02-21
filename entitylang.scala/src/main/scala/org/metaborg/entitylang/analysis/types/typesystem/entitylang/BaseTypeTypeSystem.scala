package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveType
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveType.{Boolean0, Float0, Int0, String0}

object BaseTypeTypeSystem extends SimpleTypeSystem[SPrimitiveType, BaseType](implicit typeSystem => {
  case Boolean0(origin) => rule.success(boolean)
  case Int0(origin) => rule.success(int)
  case Float0(origin) => rule.success(float)
  case String0(origin) => rule.success(string)
})