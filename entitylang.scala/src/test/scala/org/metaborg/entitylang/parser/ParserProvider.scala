package org.metaborg.entitylang.parser

import org.metaborg.scalaterms.{STerm, TermLike, TermLikeCompanion}

trait ParserProvider{
  type StartSymbol[T] <: org.metaborg.entitylang.parser.StartSymbol[T]
  def parserFor[T <: TermLike](companion: TermLikeCompanion[T], startSymbol: StartSymbol[T]): Parser[T]
}