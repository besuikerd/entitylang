import org.metaborg.entitylang.analysis.{Analyzer, AttributeNodeData}
import org.metaborg.entitylang.parser.EntityLangParserProvider
import org.scalatest.FlatSpec

class GenerateSpec extends FlatSpec{


  import org.metaborg.entitylang.generate.js.entity


  val ast = EntityLangParserProvider.parser.parseResource("/forum.etl")
  val model = Analyzer.analyze(ast)
}
