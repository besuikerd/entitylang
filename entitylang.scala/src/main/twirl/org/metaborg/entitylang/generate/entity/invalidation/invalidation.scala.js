@import org.metaborg.entitylang.analysis.EntityFieldNode
@import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds

@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyZero
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrMore
@import org.metaborg.entitylang.analysis.types.multiplicity.OneOrMore
@import org.metaborg.entitylang.generate.entity.invalidation.js.invalidateField

@import org.metaborg.entitylang.analysis.Analyzer.InvalidationFunction
@(invalidationFunction: InvalidationFunction)

export function invalidate@{invalidationFunction.field.entity}_@{invalidationFunction.field.name}(state, id){
  @if(invalidationFunction.isDerivedValue){
  state = state.update('@{invalidationFunction.field.entity}_@{invalidationFunction.field.name}', set => set.remove(id));
  }
  @for(invalidation <- invalidationFunction.invalidations){
    @invalidateField(invalidation)
  }
  return state;
}
