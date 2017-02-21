package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.{AnalysisModel, Analyzer}
import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.multiplicity.{ExactlyOne, MultiplicityBounds}
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.ExpressionTypeSystem
import org.metaborg.entitylang.analysis.types.typesystem.error.TypeError
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
import org.metaborg.entitylang.parser.{EntityLangParserProvider, SpoofaxParser}
import org.scalatest.{Assertion, FlatSpec}

import scala.reflect.ClassTag
import scalax.collection.GraphPredef

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

    fieldAssert("A", "a")(int.one)
    fieldAssert("A", "b")(int.one)
    fieldAssert("A", "c")(boolean.one)
    fieldAssert("A", "d")(int.one)
    fieldAssert("A", "e")(int.one)
    fieldAssert("A", "f")(boolean.one)
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
    val fieldAssert = createFieldAssert(program)

    fieldAssert("A", "a")(int.one)
    fieldAssert("A", "b")(boolean.one)
    fieldAssert("A", "c")(int.one)
    fieldAssert("A", "d")(int.one)

    fieldAssert("B", "a")(int.one)
    fieldAssert("B", "b")(boolean.one)
    fieldAssert("B", "c")(int.one)
    fieldAssert("B", "d")(boolean.one)
  }

  it should "typecheck cyclic dependencies" in {
    val program =
      """
        |entity A{
        |  amount: Int
        |  weight: Float
        |  value: Float = children.value + amount * weight
        |}
        |
        |relation A.parent 1 <-> * A.children
      """.stripMargin
    val fieldAssert = createFieldAssert(program)
    fieldAssert("A", "amount")(int.one)
    fieldAssert("A", "weight")(float.one)
    fieldAssert("A", "value")(float.one)
  }

  def createFieldAssert(program: String): (String, String) => (Type) => Assertion = {
    val ast = EntityLangParserProvider.parser.parse(program)
    val model = Analyzer.analyze(ast)
    if(model.errors.isEmpty)
      assertFieldType(model)
    else
      (_, _) => (_) => fail(model.errors.head.message)
  }

  def assertFieldType(model: AnalysisModel)(entity: String, field: String)(t: Type) = model.fields.collectFirst{
    case (fieldNode, dataNode) if fieldNode.entity == entity && fieldNode.name == field => dataNode.fieldType
  } match{
    case Some(t2) =>
      assertResult(t)(t2)
    case None => fail(s"Field not found: $entity.$field")
  }


  "Expressions" should "typecheck" in {
    assertType("if(true) false else true")(boolean.one)
    illTyped("if(true) 3 else false")

    assertType("!true")(boolean.one)
    assertType("!false")(boolean.one)

    assertType("1 + 2")(int.one)
    assertType("1.0 + 2.0")(float.one)
    assertType("1 + 1.0")(float.one)
    assertType("1.0 + 1")(float.one)
    illTyped("1 + true")
    illTyped("true + 1")


    assertType("5 > 4")(boolean.one)
    illTyped("true > false")

    assertType("max(2)")(int.one)
    illTyped("f(true)")

    assertType("""epic(2)""")(string ~>: boolean ~>: int)
    assertType("""epic(2 "epic")""")(boolean ~>: int)
    assertType("""epic(2 "epic" true)""")(int.one)
    illTyped("""epic(2 "epic" true 4)""")

  }

  def inferType(exp: SExp): Either[TypeError, Type] =
    ExpressionTypeSystem.typeSystem.infer(exp)

  def parseError(cause: SpoofaxParser.Error) = fail("Parse error: " + cause)

  def parse(exp: String): Either[SpoofaxParser.Error, SExp] = EntityLangParserProvider.expParser.tryParse(exp)

  def inferType(exp: String): Either[TypeError, Type] = parse(exp).fold(parseError, inferType)

  def wellTyped(exp: String) =
    inferType(exp).left.foreach(typeError)

  def illTyped(exp: String) =
    inferType(exp).right.foreach(t => fail(s"SExp is well typed: $t"))

  def typeError(t: TypeError) = fail("type error @ " + t.errorString)

  def assertType(exp: String)(t: Type) = {
    import typesystem._
    implicit val ts = ExpressionTypeSystem.typeSystem
    implicit val pp: (Type) => String = Type.ppType
    val r = for {
      e <- parse(exp).left.map(e => "Parse error: " + e.mkString("\n")).right
      u <- e.infer.ofType(t).run.left.map(e => "type error @ " + e.errorString).right
    } yield u
    r.fold[Unit](s => this.fail(s), e => {})
  }
}
