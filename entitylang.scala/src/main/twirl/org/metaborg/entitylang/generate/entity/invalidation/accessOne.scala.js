@import org.metaborg.entitylang.analysis.EntityFieldNode
@(field: EntityFieldNode)
entities = state.@{field.entity}_@{field.name}.get(entities);