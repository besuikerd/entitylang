package org.metaborg.entitylang.parser

import org.metaborg.scalaterms.{STerm, TermLike, TermLikeCompanion}

trait ParserProvider{
  type StartSymbol <: org.metaborg.entitylang.parser.StartSymbol
  def parserFor[T <: TermLike](companion: TermLikeCompanion[T], startSymbol: StartSymbol): Parser[T]
}