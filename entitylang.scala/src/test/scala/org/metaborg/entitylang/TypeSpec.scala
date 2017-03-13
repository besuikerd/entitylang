package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.{AnalysisModel, Analyzer}
import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.multiplicity._
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.ExpressionTypeSystem
import org.metaborg.entitylang.analysis.types.typesystem.error.TypeError
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.TypingRule
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
import org.metaborg.entitylang.parser.{EntityLangParserProvider, SpoofaxParser}
import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.{Assertion, FlatSpec}

import scala.reflect.ClassTag
import scalax.collection.GraphPredef

class TypeSpec extends FlatSpec{

  "Multiplicity bounds" should "have a correct partial ordering" in {
    assertResult(zeroToMany)(MultiplicityBounds.lub(zeroToMany, zeroToMany))
    assertResult(oneToMany)(MultiplicityBounds.lub(oneToMany, oneToMany))
    assertResult(zeroToOne)(MultiplicityBounds.lub(zeroToOne, zeroToOne))
    assertResult(oneToOne)(MultiplicityBounds.lub(oneToOne, oneToOne))
    assertResult(zeroToZero)(MultiplicityBounds.lub(zeroToZero, zeroToZero))

    assertResult(zeroToMany)(MultiplicityBounds.lub(zeroToMany, oneToMany))
    assertResult(zeroToMany)(MultiplicityBounds.lub(zeroToMany, zeroToOne))
    assertResult(zeroToMany)(MultiplicityBounds.lub(zeroToMany, oneToOne))
    assertResult(zeroToMany)(MultiplicityBounds.lub(zeroToMany, zeroToZero))
    assertResult(zeroToMany)(MultiplicityBounds.lub(oneToMany, zeroToOne))
    assertResult(oneToMany)(MultiplicityBounds.lub(oneToMany, oneToOne))
    assertResult(zeroToMany)(MultiplicityBounds.lub(oneToMany, zeroToZero))
    assertResult(zeroToOne)(MultiplicityBounds.lub(zeroToOne, oneToOne))
    assertResult(zeroToOne)(MultiplicityBounds.lub(zeroToOne, zeroToZero))
    assertResult(zeroToOne)(MultiplicityBounds.lub(oneToOne, zeroToZero))
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
    val fieldAssert = createFieldAssert(program)

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
        |entity F {
        | a: Int
        | b: Boolean
        | c = if(g.b) 4 else a
        | d = g.a + a > 4
        |}
        |
        |entity G {
        | a: Int
        | b: Boolean
        | c = if(sum(fs.a) > 0) fs.a else a
        | d = if(conj(fs.b)) 1 else a
        |}
        |
        |relation F.g 1 <-> * G.fs
      """.stripMargin
    val fieldAssert = createFieldAssert(program)

    fieldAssert("F", "a")(int.one)
    fieldAssert("F", "b")(boolean.one)
    fieldAssert("F", "c")(int.one)
    fieldAssert("F", "d")(boolean.one)

    fieldAssert("G", "a")(int.one)
    fieldAssert("G", "b")(boolean.one)
    fieldAssert("G", "c")(int.*)
    fieldAssert("G", "d")(int.one)
  }

  it should "typecheck cyclic dependencies" in {
    val program =
      """
        |entity A{
        |  amount: Int
        |  weight: Float
        |  value: Float = sum(children.value) + amount * weight
        |}
        |
        |entity B {
        |  a: Int = b
        |  b = c + d
        |  c: Int = d
        |  d = a
        |}
        |
        |relation A.parent 1 <-> * A.children
      """.stripMargin

    val fieldAssert = createFieldAssert(program)
    fieldAssert("A", "amount")(int.one)
    fieldAssert("A", "weight")(float.one)
    fieldAssert("A", "value")(float.one)
    fieldAssert("A", "children")(EntityType("A") withMultiplicity zeroToMany)

    fieldAssert("B", "a")(int.one)
    fieldAssert("B", "b")(int.one)
    fieldAssert("B", "c")(int.one)
    fieldAssert("B", "d")(int.one)
  }

  "ChoiceLeft" should "infer correct types" in {
    val program =
      """
        |entity A {
        |  a: Int?
        |  b = a <+ 5
        |  c = a <+ null
        |  d = c <+ b
        |}
      """.stripMargin
    val fieldAssert = createFieldAssert(program)
    fieldAssert("A", "a")(int.?)
    fieldAssert("A", "b")(int.one)
    fieldAssert("A", "c")(int.?)
    fieldAssert("A", "d")(int.one)
  }

  "Merge" should "infer correct types" in {
    val program =
      """
        |entity A {
        |  a: Int?
        |  b: Int
        |  c: Int*
        |  d: Int+
        |  e = a ++ b
        |  f = b ++ b
        |  g = a ++ a
        |  h = d ++ c
        |  i = null ++ a
        |  j = null ++ b
        |  k = null ++ c
        |  l = null ++ d
        |}
      """.stripMargin
    val fieldAssert = createFieldAssert(program)

    fieldAssert("A", "a")(int.?)
    fieldAssert("A", "b")(int.one)
    fieldAssert("A", "c")(int.*)
    fieldAssert("A", "d")(int.+)
    fieldAssert("A", "e")(int.+)
    fieldAssert("A", "f")(int.+)
    fieldAssert("A", "g")(int.*)
    fieldAssert("A", "h")(int.+)
    fieldAssert("A", "i")(int.?)
    fieldAssert("A", "j")(int.one)
    fieldAssert("A", "k")(int.*)
    fieldAssert("A", "l")(int.+)
  }

  def createFieldAssert(program: String): (String, String) => (Type) => Assertion = {
    val ast = EntityLangParserProvider.parser.parse(program)
    val model = Analyzer.analyze(ast)
    if(model.errors.isEmpty)
      assertFieldType(model)
    else
      (_, _) => (_) => fail(
        s"The following type errors occurred on the model:\n${model.errors.map(e => "  " + (if(e.origin == null) "[missing origin]" else s"[${e.origin.filename} ${e.origin.line}:${e.origin.column}, ${e.origin.startOffset}-${e.origin.endOffset}]") + s": ${e.message}").mkString("\n")}")
  }

  def assertFieldType(model: AnalysisModel)(entity: String, field: String)(t: Type) = model.fields.collectFirst{
    case (fieldNode, dataNode) if fieldNode.entity == entity && fieldNode.name == field => dataNode.fieldType
  } match{
    case Some(t2) =>
      assertResult(t)(t2)(prettifier = Prettifier({case t: Type => Type.ppType(t)}), implicitly[Position])
    case None => fail(s"Field not found: $entity.$field")
  }


  "Expressions" should "typecheck" in {

    nTypeErrors("true + false")(2)

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
    assertType(""" "foo" + "bar" """)(string.one)
    assertType(""" "foo" + 1 """)(string.one)
    assertType(""" "foo" + true """)(string.one)
    illTyped(""" 1 + "bar" """)


    assertType("5 > 4")(boolean.one)
    illTyped("true > false")

    assertType("max(2)")(int.one)
    illTyped("f(true)")
  }

  def inferType(exp: SExp): TypingRule.Aux[SExp, Type, Type]#TypingResult =
    ExpressionTypeSystem.infer(exp)

  def parseError(cause: SpoofaxParser.Error) = fail("Parse error: " + cause)

  def parse(exp: String): Either[SpoofaxParser.Error, SExp] = EntityLangParserProvider.expParser.tryParse(exp)

  def inferType(exp: String): TypingRule.Aux[SExp, Type, Type]#TypingResult = parse(exp).fold(parseError, inferType)

  def wellTyped(exp: String) =
    inferType(exp).left.foreach(_.foreach(typeError))

  def illTyped(exp: String) =
    inferType(exp).right.foreach(t => fail(s"SExp is well typed: $t"))

  def nTypeErrors(exp:String)(n: Int) =
    inferType(exp).fold(errors => if(errors.length != n) fail(s"expected $n type errors, got: ${errors.length}"), t => fail(s"SExp is well typed: $t"))

  def typeError(t: TypeError) = fail("type error @ " + t.errorString)

  def assertType(exp: String)(t: Type) = {
    import typesystem._
    implicit val ts = ExpressionTypeSystem
    implicit val pp: (Type) => String = Type.ppType
    val r = for {
      e <- parse(exp).left.map(e => "Parse error: " + e.mkString("\n")).right
      u <- e.infer.ofType(t).run.left.map(e => "type errors @ " + e.map(_.errorString).mkString(", ")).right
    } yield u
    r.fold[Unit](s => this.fail(s), e => {})
  }
}
