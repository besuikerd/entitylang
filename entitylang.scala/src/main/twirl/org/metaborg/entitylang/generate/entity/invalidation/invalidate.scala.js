@import org.metaborg.entitylang.analysis.EntityFieldNode
@import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyZero
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrMore
@import org.metaborg.entitylang.analysis.types.multiplicity.OneOrMore
@import org.metaborg.entitylang.generate.entity.invalidation.js.{invalidateOption, invalidateOne, invalidateMany}

@import org.metaborg.entitylang.analysis.Analyzer.FieldWithMultiplicity
@(field: FieldWithMultiplicity)
@{field.multiplicity match {
  case ExactlyZero() =>
  case ZeroOrOne() => invalidateOption(field.field)
  case ExactlyOne() => invalidateOne(field.field)
  case ZeroOrMore() => invalidateMany(field.field)
  case OneOrMore() => invalidateMany(field.field)
}}