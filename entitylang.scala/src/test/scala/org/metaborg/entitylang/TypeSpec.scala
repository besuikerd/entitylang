package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem.error.TypeError
import org.metaborg.entitylang.analysis.{Analyzer, Old_DataflowAnalysis, Old_TypeAnalysis}
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
import org.metaborg.entitylang.parser.{EntityLangParserProvider, SpoofaxParser}
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
    assertType("if(true) false else true")(boolean)
    illTyped("if(true) 3 else false")
  }



  def inferType(exp: SExp): Either[TypeError, Type] =
    EntityLangTypeSystem.typeSystem.infer(exp)

  def parseError(cause: SpoofaxParser.Error) = fail("Parse error: " + cause)

  def parse(exp: String): Either[SpoofaxParser.Error, SExp] = EntityLangParserProvider.expParser.tryParse(exp)

  def inferType(exp: String): Either[TypeError, Type] = parse(exp).fold(parseError, inferType)

  def wellTyped(exp: String) =
    inferType(exp).left.foreach(typeError)

  def illTyped(exp: String) =
    inferType(exp).right.foreach(t => fail(s"SExp is well typed: $t"))

  def typeError(t: TypeError) = fail("type error @ " + t.errorString)

  def assertType(exp: String)(t: Type) = inferType(exp).fold(typeError, t2 => if(t2 != t) fail(s"Expected type: $t, got: $t2"))
}
