@import org.metaborg.entitylang.analysis.EntityFieldNode

@(field: EntityFieldNode)
state = invalidate@{field.entity}_@{field.name}(state, entities);