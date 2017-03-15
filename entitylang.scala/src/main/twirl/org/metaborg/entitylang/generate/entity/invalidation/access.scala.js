@import org.metaborg.entitylang.analysis.EntityFieldNode
@import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyZero
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrMore
@import org.metaborg.entitylang.analysis.types.multiplicity.OneOrMore
@import org.metaborg.entitylang.generate.entity.invalidation.js.{accessZero, accessOption, accessOne, accessMany}

@(field: EntityFieldNode, multiplicity: MultiplicityBounds)
@{multiplicity match {
  case ExactlyZero() => accessZero(field)
  case ZeroOrOne() => accessOption(field)
  case ExactlyOne() => accessOne(field)
  case ZeroOrMore() => accessMany(field)
  case OneOrMore() => accessMany(field)
}}