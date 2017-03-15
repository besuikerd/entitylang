@import org.metaborg.entitylang.analysis.EntityFieldNode
@(field: EntityFieldNode)
entities = entities.map(state.@{field.entity.capitalize}_@{field.name.capitalize}.get);