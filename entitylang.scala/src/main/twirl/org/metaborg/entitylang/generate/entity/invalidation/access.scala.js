@import org.metaborg.entitylang.analysis.Analyzer.PathField
@import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyZero
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ExactlyOne
@import org.metaborg.entitylang.analysis.types.multiplicity.ZeroOrMore
@import org.metaborg.entitylang.analysis.types.multiplicity.OneOrMore
@import org.metaborg.entitylang.generate.entity.invalidation.js.{accessZero, accessManyOne, accessManyOption, accessManyMany, accessOne, accessOption}
@import play.twirl.api._
@(path: Seq[PathField], continuation: => Appendable[JavaScript])
@{path match {
  case field +: remainder =>
    def template(field: PathField, continuation: => Appendable[JavaScript]) =
      (field.previousMultiplicity, field.nextMultiplicity) match {
        case (_, ExactlyZero()) => accessZero(field.field, continuation)
        case (OneOrMore() | ZeroOrMore(), m2) => m2 match{
          case ExactlyOne() => accessManyOne(field.field, continuation)
          case ZeroOrOne() => accessManyOption(field.field, continuation)
          case OneOrMore() | ZeroOrMore() => accessManyMany(field.field, continuation)
        }
        case (ExactlyOne(), _) => accessOne(field.field, continuation)
        case (ZeroOrOne(), _) => accessOption(field.field, continuation)
        case (ExactlyZero(), _) => accessZero(field.field, continuation)
      }
    template(field, access(remainder, continuation))
  case _ => continuation
}}