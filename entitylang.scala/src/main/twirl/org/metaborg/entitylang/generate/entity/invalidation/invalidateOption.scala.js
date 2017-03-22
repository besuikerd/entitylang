@import org.metaborg.entitylang.analysis.EntityFieldNode

@(field: EntityFieldNode)
if(entities !== null){
  state = invalidate@{field.entity}_@{field.name}(state, entities);
}
