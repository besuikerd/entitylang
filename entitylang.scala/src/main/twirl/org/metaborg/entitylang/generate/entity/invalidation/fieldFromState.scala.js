@import org.metaborg.entitylang.analysis.EntityFieldNode
@(field: EntityFieldNode)
state.get('@{field.entity}_@{field.name}')