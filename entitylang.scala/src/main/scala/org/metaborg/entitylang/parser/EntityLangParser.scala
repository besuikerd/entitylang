package org.metaborg.entitylang.parser

import java.io.FileNotFoundException

import org.metaborg.scalaterms.{STerm, TermLike, TermLikeCompanion}
import org.metaborg.spoofax.core.Spoofax
import org.metaborg.spoofax.core.syntax.JSGLRParserConfiguration
import org.metaborg.spoofax.core.unit.{InputContrib, InputUnit}
import scala.collection.JavaConversions._
import scala.io.Source

trait EntityLangParser {
  private val spoofax = new Spoofax()
  private val languageLocation = spoofax.resourceService.resolve("../entitylang.lang")
  private val languageRequest = spoofax.languageDiscoveryService.request(languageLocation)
  private val lang = spoofax.languageDiscoveryService.discover(languageRequest).iterator().next()
  private val langImpl = spoofax.languageService.getImpl(lang.config().identifier())

  def tryParse(input: String): Either[Seq[String], STerm] = {
    val cfg = new JSGLRParserConfiguration("Start")
    val contrib = new InputContrib(input, langImpl, null, cfg)
    val inputUnit = new InputUnit(new org.metaborg.spoofax.core.unit.Unit(null), contrib)
    val parseResult = spoofax.syntaxService.parse(inputUnit)
    if(parseResult.success()){
      Right(STerm.fromStratego(parseResult.ast()))
    } else{
      Left((for(m <- parseResult.messages()) yield m.message()).toSeq)
    }
  }

  def parseTerm[T <: TermLike](t: TermLikeCompanion[T])(input: String): T = {
    parse(input){
      case t.fromSTerm(t) => t
    }
  }

  def parse[R](input: String)(pf: PartialFunction[STerm, R]): R = {
    tryParse(input) match{
      case Right(term) => pf(term)
      case Left(errors) => throw new Error(errors.mkString("\n"))
    }
  }

  def parseFile[R](path: String)(pf: PartialFunction[STerm, R]): R = {
    val resource = getClass.getResource(path)
    if(resource == null) {
      throw new FileNotFoundException("resource " + path)
    }
    parse(Source.fromURL(resource).mkString)(pf)
  }
}
