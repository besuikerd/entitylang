import org.metaborg.entitylang.analysis.{Analyzer, AttributeNodeData, EntityNode}
import org.metaborg.entitylang.parser.EntityLangParserProvider
import org.scalatest.FlatSpec

class GenerateSpec extends FlatSpec{


  import org.metaborg.entitylang.generate.js.entity


  val ast = EntityLangParserProvider.parser.parseResource("/forum.etl")
  val model = Analyzer.analyze(ast)

  for(e <- model.graph.entities){
    val fields = e.attributesOfEntity.map(n => model.fields(n).asInstanceOf[AttributeNodeData])

    println(entity(e.typedValue[EntityNode].name, fields))
  }


}
