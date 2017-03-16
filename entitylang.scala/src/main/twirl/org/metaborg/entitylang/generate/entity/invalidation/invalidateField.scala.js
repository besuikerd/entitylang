@import org.metaborg.entitylang.analysis.EntityFieldNode
@import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds

@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyZero
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrMore
@import org.metaborg.entitylang.analysis.types.multiplicity.OneOrMore
@import org.metaborg.entitylang.generate.entity.invalidation.js.{access, invalidate}

@import org.metaborg.entitylang.analysis.Analyzer.FieldInvalidation
@import org.metaborg.entitylang.analysis.Analyzer.FieldWithMultiplicity
@(fieldInvalidation: FieldInvalidation)
{
  //@{fieldInvalidation.field.field.entity}.@{fieldInvalidation.field.field.name}
  let entities = id;
  @access(fieldInvalidation.path, invalidate(fieldInvalidation.field))
}