@import org.metaborg.entitylang.analysis.EntityFieldNode

@(field: EntityFieldNode)
entities.forEach(id => state = invalidate@{field.entity}_@{field.name}(state, id));