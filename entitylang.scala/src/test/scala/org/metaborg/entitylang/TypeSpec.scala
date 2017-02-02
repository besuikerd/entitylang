package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.TypeAnalysis
import org.metaborg.entitylang.analysis.TypeAnalysis.FunctionType
import org.scalatest.FlatSpec

class TypeSpec extends FlatSpec{


  "merge operator" should " merge multiplicities correctly" in {

    import org.metaborg.entitylang.analysis.TypeAnalysis._
    val multiplicities: Seq[BaseMultiplicityType] = Seq(One, ZeroOrOne, ZeroOrMore, OneOrMore)

    val merged = for{
      c <- multiplicities
      r <- multiplicities
    } yield (c, r, MultiplicityType.merge(c, r))

    merged.foreach{case (m1, m2, merged) => println(f"$m1%20s ++ $m2%-20s => $merged%-20s")}
  }

  "type checker" should "infer types of expressions correctly" in {
    import TypeAnalysis._
    import org.metaborg.entitylang.parser.EntityLangParserProvider.expParser._

    import Type._

    val env = Map(
      "f" -> boolean ~>: string
    )

    println(int ~>: string ~>: boolean)

    println(TypeAnalysis.getType(env)(parse("f(true)")))

  }
}
