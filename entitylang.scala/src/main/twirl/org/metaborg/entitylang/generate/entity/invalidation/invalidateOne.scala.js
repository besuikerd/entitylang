@import org.metaborg.entitylang.analysis.EntityFieldNode

@(field: EntityFieldNode)
state = invalidate@{field.entity.capitalize}_@{field.name.capitalize}(entities)