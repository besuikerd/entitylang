package org.metaborg.entitylang.analysis.types.typesystem.entitylang



import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.multiplicity._
import org.metaborg.entitylang.lang.ast.MType.SMultiplicity
import org.metaborg.entitylang.lang.ast.MType.SMultiplicity.{One0, OneOrMore0, ZeroOrMore0, ZeroOrOne0}

object MultiplicityTypeSystem extends SimpleTypeSystem[SMultiplicity, MultiplicityBounds](implicit typeSystem => {
  case One0(origin) => typeRule.success(oneToOne)
  case ZeroOrOne0(origin) => typeRule.success(zeroToOne)
  case ZeroOrMore0(origin) => typeRule.success(zeroToMany)
  case OneOrMore0(origin) => typeRule.success(oneToMany)
})
