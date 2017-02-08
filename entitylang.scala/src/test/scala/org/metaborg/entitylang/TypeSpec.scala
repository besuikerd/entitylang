package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.{Analyzer, Old_DataflowAnalysis, Old_TypeAnalysis}
import org.metaborg.entitylang.analysis.Old_TypeAnalysis.FunctionType
import org.metaborg.entitylang.parser.EntityLangParserProvider
import org.scalatest.FlatSpec

import scala.reflect.ClassTag

class TypeSpec extends FlatSpec{


  "merge operator" should " merge multiplicities correctly" in {

    import org.metaborg.entitylang.analysis.Old_TypeAnalysis._
    val multiplicities: Seq[BaseMultiplicityType] = Seq(One, ZeroOrOne, ZeroOrMore, OneOrMore)

    val merged = for{
      c <- multiplicities
      r <- multiplicities
    } yield (c, r, MultiplicityType.merge(c, r))

    merged.foreach{case (m1, m2, merged) => println(f"$m1%20s ++ $m2%-20s => $merged%-20s")}
  }

  "strongly connected components" should " be calculated correctly" in {
    val p = EntityLangParserProvider.parser.parseResource("/test.etl")
    val dataflowGraph = Old_DataflowAnalysis.dataflowAnalysis(p)
    val components = Old_DataflowAnalysis.stronglyConnected(dataflowGraph)

    import scalax.collection.io.json._
    import scalax.collection.io.json.descriptor.predefined.UnDi


    import analysis._

    def descriptor[T: Manifest](id: String, f: T => String): NodeDescriptor[T] = new NodeDescriptor[T](typeId = id){
      override def id(node: Any): String = f(node.asInstanceOf[T])
    }

    val entityDescriptor = descriptor[EntityNode]("entity", _.name)
    val attributeDescriptor = descriptor[AttributeNode]("attribute", n => s"${n.entity}.${n.name}")
    val derivedValueDescriptor = descriptor[DerivedValueNode]("derivedValue", n => s"${n.entity}.${n.name}")
    val relationDescriptor = descriptor[RelationNode]("relation", n => s"${n.entity}.${n.name}")

    val dataflowGraphDescriptor = new Descriptor[AnalysisGraphNode](
      defaultNodeDescriptor = entityDescriptor,
      defaultEdgeDescriptor = UnDi.descriptor(),
      namedNodeDescriptors = Seq(
        attributeDescriptor,
        derivedValueDescriptor,
        relationDescriptor
      )
    )


    val model = Analyzer.analyze(p)

    val js = model.graph.toJson(dataflowGraphDescriptor)
    println(js)


//    for {
//      (component, i) <- components.zipWithIndex
//      node <- {
//        println("Component " + i)
//        component
//      }
//    } println(node)
  }

  "type checker" should "infer types of expressions correctly" in {
    import Old_TypeAnalysis._
    import org.metaborg.entitylang.parser.EntityLangParserProvider.expParser._

    import Type._

    val env = Map(
      "f" -> boolean ~>: string
    )

    println(int ~>: string ~>: boolean)

    println(Old_TypeAnalysis.getType(env)(parse("f(true)")))

  }
}
