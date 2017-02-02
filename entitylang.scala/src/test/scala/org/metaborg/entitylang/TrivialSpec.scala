package org.metaborg.entitylang

import org.metaborg.entitylang.desugar.{Add, BinExp}
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.scalatest.FlatSpec


class TrivialSpec extends FlatSpec
{
  "this" should "always succeed" in {

    import org.metaborg.entitylang.parser.EntityLangParserProvider.parser._

    import org.metaborg.entitylang.parser.EntityLangParserProvider.expParser

    parseResource("/test.etl") match {
      case s@Start1(_, _) => println(s)
    }

    expParser.parse("1 + 1") match {
      case BinExp(Add, e1, e2) => println(s"adding $e1 and $e2")
    }





    assertResult(42)(42)
  }
}
