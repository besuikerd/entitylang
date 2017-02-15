package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.types.TypeSystem
import org.metaborg.entitylang.analysis.{Analyzer, Old_DataflowAnalysis, Old_TypeAnalysis}
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
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

//    val add = num("x") =>: num("y") =>: "x" ~>: "y" ~>: lub("x", "y")
    val identity = "x" ~>: "x"

    println(add)
    println(ppType(add))

    implicit val env = TypingEnvironment.apply()

//    println(ppType(reduceByApplication(add, int)))
//    println(ppType(reduceByApplication(add, string)))
    println(ppType(TypeWithEnvironment.reduce(identity, int)))


    val add1 = TypeWithEnvironment.reduce(add, string)
    val add2 = add1.reduce(float)
    println(ppType(add1))
    println(ppType(add2))
  }

  "test type rules" should "test" in {
    val e = EntityLangParserProvider.expParser.parse("if(true) false else true")

    e match{
      case ifExp: If3 =>
        val res = TypeSystem.if3(ifExp).run
        println(res)
      case _ =>
    }
  }
}
