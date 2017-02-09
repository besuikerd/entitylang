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

    val program =
      """
        |entity A {
        |  c = a + b
        |  a: Int
        |  b: Int
        |  d = e + 1
        |  e = d + 1
        |}
      """.stripMargin
    val ast = EntityLangParserProvider.parser.parseResource("/test.etl")
    val model = Analyzer.analyze(ast)
    val scc = model.graph.scc


    val cycles = scc.map(n => n.innerNodeTraverser.findCycle.map(_.nodes.toList.distinct).getOrElse(Seq(n)))

    cycles.foreach(println)

    println("---")

    scc.foreach(println)
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
