package org.metaborg.entitylang.parser

import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart

sealed abstract class EntityLangStartSymbol[T](startSymbol: String) extends StartSymbol[T]{
  override def stringRepresentation: String = startSymbol
}
case object Exp extends EntityLangStartSymbol[SExp]("Exp")
case object Start extends EntityLangStartSymbol[SStart]("Start")

object EntityLangParserProvider extends ParserProvider with SpoofaxParserProvider{
  override def languageLocation: String = "../entitylang.lang"
  override type StartSymbol[T] = EntityLangStartSymbol[T]

  val expParser = parserFor(SExp, Exp)
  val parser = parserFor(SStart, Start)
}
