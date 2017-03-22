package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

case class TypingResult[TermType <: HasOrigin, +TypeType, +T](t: T, subTypes: Map[TermType, TypeType])