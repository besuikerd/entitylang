@import org.metaborg.entitylang.analysis.EntityFieldNode
@(field: EntityFieldNode)
entities = state.@{field.entity.capitalize}_@{field.name.capitalize}.get(entities);