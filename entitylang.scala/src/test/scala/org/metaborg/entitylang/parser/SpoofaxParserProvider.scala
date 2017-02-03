package org.metaborg.entitylang.parser

import org.metaborg.scalaterms.{TermLike, TermLikeCompanion}
import org.metaborg.spoofax.core.Spoofax

trait SpoofaxParserProvider extends ParserProvider {
  def languageLocation: String

  private val spoofax = new Spoofax()
  private val languagePath = spoofax.resourceService.resolve(languageLocation)
  private val languageRequest = spoofax.languageDiscoveryService.request(languagePath)
  private val lang = spoofax.languageDiscoveryService.discover(languageRequest).iterator().next()
  private val langImpl = spoofax.languageService.getImpl(lang.config().identifier())

  override def parserFor[T <: TermLike](companion: TermLikeCompanion[T], startSymbol: StartSymbol): Parser[T] = new SpoofaxParser[T](
    startSymbol = startSymbol,
    languageImpl = langImpl,
    syntaxService = spoofax.syntaxService,
    companion = companion
  )
}