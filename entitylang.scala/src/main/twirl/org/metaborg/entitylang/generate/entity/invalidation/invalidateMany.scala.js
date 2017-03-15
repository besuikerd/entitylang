@import org.metaborg.entitylang.analysis.EntityFieldNode

@(field: EntityFieldNode)
for(let i = 0 ; i < entities.length ; i++){
  state = invalidate@{field.entity.capitalize}_@{field.name.capitalize}(state, entities[i])
}