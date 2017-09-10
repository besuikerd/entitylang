package org.metaborg.entitylang.parser

import org.metaborg.scalaterms.{TermLike, TermLikeCompanion}

trait ParserProvider[Error]{
  type StartSymbol <: org.metaborg.entitylang.parser.StartSymbol
  def parserFor[T <: TermLike](companion: TermLikeCompanion[T], startSymbol: StartSymbol): Parser[T, Error]
}