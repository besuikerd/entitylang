package org.metaborg.entitylang

import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.scalatest.FlatSpec


class TrivialSpec extends FlatSpec with ParserFixture{
  "this" should "always succeed" in {

    parseFile("/test.etl"){
      case Start1.fromSTerm(s) => println(s)
    }


    val pStart = parseTerm(Start1) _
    val t = pStart("entity Student{}")

    println(t)
    assertResult(42)(42)
  }
}
