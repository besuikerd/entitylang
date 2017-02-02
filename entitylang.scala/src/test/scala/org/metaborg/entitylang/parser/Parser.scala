package org.metaborg.entitylang.parser

import java.io.File

import org.metaborg.scalaterms.TermLike

import scala.io.Source

trait Parser[T <: TermLike]{
  type Error

  val startSymbol: StartSymbol[T]

  private def unsafeExtractResult(r: Either[Error, T]): T = r.right.get

  def tryParse(input: String): Either[Error, T]

  def parse(input: String): T = unsafeExtractResult(tryParse(input))

  def tryParseFile(path: String): Either[Error, T] = {
    val source = Source.fromFile(new File(path)).mkString
    tryParse(source)
  }

  def parseFile(path: String): T = unsafeExtractResult(tryParseFile(path))

  def tryParseResource(path: String): Either[Error, T] = {
    val source = Source.fromURL(getClass.getResource(path)).mkString
    tryParse(source)
  }

  def parseResource(path: String): T = unsafeExtractResult(tryParseResource(path))
}
