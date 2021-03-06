@import org.metaborg.entitylang.analysis.EntityFieldNode
@import org.metaborg.entitylang.analysis.Analyzer.PathField
@import org.metaborg.entitylang.generate.entity.invalidation.js.fieldFromState
@import org.metaborg.entitylang.generate.entity.invalidation.js.access
@import play.twirl.api._
@(field: EntityFieldNode, continuation: => Appendable[JavaScript])
entities = entities.map(@{fieldFromState(field)}.get);
@continuation