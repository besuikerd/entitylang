@import org.metaborg.entitylang.analysis.EntityFieldNode

@(field: EntityFieldNode)
if(entities !== null){
  state = invalidate@{field.entity.capitalize}_@{field.name.capitalize}(state, entities)
}
