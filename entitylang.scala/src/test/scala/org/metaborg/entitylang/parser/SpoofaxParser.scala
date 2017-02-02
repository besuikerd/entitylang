package org.metaborg.entitylang.parser

import org.metaborg.core.language.ILanguageImpl
import org.metaborg.core.syntax.ISyntaxService
import org.metaborg.scalaterms.{TermLike, TermLikeCompanion}
import org.metaborg.spoofax.core.syntax.JSGLRParserConfiguration
import org.metaborg.spoofax.core.unit.{ISpoofaxInputUnit, ISpoofaxParseUnit, InputContrib, InputUnit}
import scala.collection.JavaConversions._

class SpoofaxParser[T <: TermLike](
  override val startSymbol: StartSymbol[T],
  languageImpl: ILanguageImpl,
  syntaxService: ISyntaxService[ISpoofaxInputUnit, ISpoofaxParseUnit],
  companion: TermLikeCompanion[T]
) extends Parser[T]{
  type Error = Seq[String]

  val cfg = new JSGLRParserConfiguration(startSymbol.stringRepresentation)

  override def tryParse(input: String): Either[Error, T] = {
    val contrib = new InputContrib(input, languageImpl, null, cfg)
    val inputUnit = new InputUnit(new org.metaborg.spoofax.core.unit.Unit(null), contrib)
    val parseResult = syntaxService.parse(inputUnit)
    if(parseResult.success()){
      companion.fromStratego.unapply(parseResult.ast()) match{
        case Some(t) => Right(t)
        case _ => Left(Seq("Could not convert ast to valid STerm representation"))
      }
    } else{
      Left((for(m <- parseResult.messages()) yield m.message()).toSeq)
    }
  }
}