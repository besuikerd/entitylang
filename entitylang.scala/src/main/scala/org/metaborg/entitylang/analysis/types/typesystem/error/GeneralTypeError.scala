package org.metaborg.entitylang.analysis.types.typesystem.error

import org.metaborg.scalaterms.Origin

case class GeneralTypeError(origin: Origin, message: String) extends TypeError