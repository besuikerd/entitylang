package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.{AnalysisModel, Analyzer}
import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.EntityLangTypeSystem
import org.metaborg.entitylang.analysis.types.typesystem.error.TypeError
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
import org.metaborg.entitylang.parser.{EntityLangParserProvider, SpoofaxParser}
import org.scalatest.FlatSpec

import scala.reflect.ClassTag

class TypeSpec extends FlatSpec{

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

    val p2 =
      """
        |entity A {
        | x: Boolean = y
        | y = x
        |}
      """.stripMargin

//    val ast = EntityLangParserProvider.parser.parse(p2)
    val ast = EntityLangParserProvider.parser.parseResource("/test.etl")
    val model = Analyzer.analyze(ast)
    val scc = model.graph.stronglyConnectedComponents

    scc.foreach(println)
  }

  "Entities" should "typecheck with single entity" in {
    val program =
      """
        |entity A {
        |  a: Int
        |  b: Int
        |  c: Boolean
        |  d = a + 1
        |  e = if(c) a else b
        |  f = !c
        |}
      """.stripMargin
    val ast = EntityLangParserProvider.parser.parse(program)
    val model = Analyzer.analyze(ast)
    val fieldAssert = assertFieldType(model) _

    fieldAssert("A", "a")(int)
    fieldAssert("A", "b")(int)
    fieldAssert("A", "c")(boolean)
    fieldAssert("A", "d")(int)
    fieldAssert("A", "e")(int)
    fieldAssert("A", "f")(boolean)
  }

  it should "typecheck with multiple entities" in {
    val program =
      """
        |entity A {
        | a: Int
        | b: Boolean
        | c = if(right.a > 1) right.a else a
        | d = if(right.b) right.a else a
        |}
        |
        |entity B {
        | a: Int
        | b: Boolean
        | c = if(left.b) 4 else a
        | d = left.a + a > 4
        |}
        |
        |relation A.right * <-> * B.left
      """.stripMargin
    val ast = EntityLangParserProvider.parser.parse(program)
    val model = Analyzer.analyze(ast)
    val fieldAssert = assertFieldType(model) _

    fieldAssert("A", "a")(int)
    fieldAssert("A", "b")(boolean)
    fieldAssert("A", "c")(int)
    fieldAssert("A", "d")(int)

    fieldAssert("B", "a")(int)
    fieldAssert("B", "b")(boolean)
    fieldAssert("B", "c")(int)
    fieldAssert("B", "d")(boolean)
  }

  def assertFieldType(model: AnalysisModel)(entity: String, field: String)(t: Type) = model.fields.collectFirst{
    case (fieldNode, dataNode) if fieldNode.entity == entity && fieldNode.name == field => dataNode.fieldType
  } match{
    case Some(t2) => assertResult(t)(t2)
    case None => fail(s"Field not found: $entity.$field")
  }


  "Expressions" should "typecheck" in {
    assertType("if(true) false else true")(boolean)
    illTyped("if(true) 3 else false")

    assertType("!true")(boolean)
    assertType("!false")(boolean)

    assertType("1 + 2")(int)
    assertType("1.0 + 2.0")(float)
    assertType("1 + 1.0")(float)
    assertType("1.0 + 1")(float)
    illTyped("1 + true")
    illTyped("true + 1")


    assertType("5 > 4")(boolean)
    illTyped("true > false")

    assertType("max(2)")(int)
    illTyped("f(true)")

    assertType("""epic(2)""")(string ~>: boolean ~>: int)
    assertType("""epic(2 "epic")""")(boolean ~>: int)
    assertType("""epic(2 "epic" true)""")(int)
    illTyped("""epic(2 "epic" true 4)""")

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
