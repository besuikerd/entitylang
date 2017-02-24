package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveType
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveType.{Boolean0, Float0, Int0, String0}

object BaseTypeTypeSystem extends SimpleTypeSystem[SPrimitiveType, BaseType](implicit typeSystem => {
  case Boolean0(origin) => typeRule.success(boolean)
  case Int0(origin) => typeRule.success(int)
  case Float0(origin) => typeRule.success(float)
  case String0(origin) => typeRule.success(string)
}){
  override def prettyPrint(t: BaseType): String = Type.ppBaseType(t)
}