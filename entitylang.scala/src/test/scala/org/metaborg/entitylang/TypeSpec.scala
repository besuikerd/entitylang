package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.{Analyzer, Old_DataflowAnalysis, Old_TypeAnalysis}
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
    val scc = model.graph.stronglyConnectedComponents


    val cycles = scc.map(n => n.innerNodeTraverser.findCycle.map(_.nodes.toList.distinct).getOrElse(Seq(n)))

    cycles.foreach(println)

    println("---")

    scc.foreach(println)
  }

  "type checker" should "infer types of expressions correctly" in {
    import analysis.types._

    val add = num("x") =>: num("y") =>: "x" ~>: "y" ~>: lub("x", "y")
    val identity = "x" ~>: "x"

    println(FunctionType(IntType(), FunctionType(StringType(), BooleanType())))

    println(int ~>: string ~>: boolean)

    println(add)
    println(ppType(add))


//    println(ppType(reduceByApplication(add, int)))
//    println(ppType(reduceByApplication(add, string)))
    println(ppType(reduceByApplication(identity, int)))
    println(ppType(substituteTypeVariable("x", identity, int)))
  }
}
