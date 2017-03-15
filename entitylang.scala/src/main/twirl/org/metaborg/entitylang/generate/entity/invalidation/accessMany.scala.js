@import org.metaborg.entitylang.analysis.EntityFieldNode
@(field: EntityFieldNode)
entities = entities.map(state.@{field.entity}_@{field.name}.get);