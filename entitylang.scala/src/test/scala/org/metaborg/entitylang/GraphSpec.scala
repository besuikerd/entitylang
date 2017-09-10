package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.Analyzer
import org.metaborg.entitylang.parser.EntityLangParserProvider
import org.scalatest.FlatSpec
import analysis._

class GraphSpec extends FlatSpec{
  val program =
    """
      |entity Entity{
      |  field1: String
      |  field2: String
      |  field3 = field1 + field2
      |}
      |
      |entity Entity2{
      |  field4: String
      |  field5: String
      |  field6 = rel2.field1 + rel2.field2
      |}
      |
      |relation Entity.rel1 1 <-> 1 Entity.rel2
    """.stripMargin
  val ast = EntityLangParserProvider.parser.parse(program)
  val model = Analyzer.analyze(ast)

  "graph" should "find different node types"  in {
    assertResult(8)(model.fields.size)
    assertResult(4)(model.attributes.size)
    assertResult(2)(model.derivedValues.size)
    assertResult(2)(model.relations.size)
  }
//  "entityfield nodes" should "be able to find their entity node" in {
//    val result = for {
//      childPass <- graph.findEntityField("Entity", "field1")
//      entity <- childPass.entityOfEntityField
//    } yield entity
//    assertResult(Some(EntityNode("Entity")))(result)
//  }
}
