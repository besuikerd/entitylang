package org.metaborg.entitylang.parser

import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1

sealed abstract class EntityLangStartSymbol(startSymbol: String) extends StartSymbol{
  override def stringRepresentation: String = startSymbol
}
case object Exp extends EntityLangStartSymbol("Exp")
case object Start extends EntityLangStartSymbol("Start")

object EntityLangParserProvider extends ParserProvider with SpoofaxParserProvider{
  override def languageLocation: String = "../entitylang.syntax"
  override type StartSymbol = EntityLangStartSymbol

  val expParser = parserFor(SExp, Exp)
  val parser = parserFor(SStart, Start).map{case s: Start1 => s}
}
