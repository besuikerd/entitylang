package org.metaborg.entitylang.parser

import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.entitylang.parser.SpoofaxParser.Error

sealed abstract class EntityLangStartSymbol(startSymbol: String) extends StartSymbol{
  override def stringRepresentation: String = startSymbol
}
case object Exp extends EntityLangStartSymbol("Exp")
case object Start extends EntityLangStartSymbol("Start")

object EntityLangParserProvider extends SpoofaxParserProvider{
  override def languageLocation: String = "../entitylang.syntax"
  override type StartSymbol = EntityLangStartSymbol

  val expParser: Parser[SExp, Error] = parserFor(SExp, Exp)
  val parser: Parser[Start1, Error] = parserFor(SStart, Start).map{case s: Start1 => s}
}