package org.metaborg.entitylang.analysis.types.typesystem.error

import org.metaborg.scalaterms.Origin

trait TypeError {
  val origin: Origin
  val message: String


  def originString = if(origin == null) "[missing origin]" else s"[${origin.filename} ${origin.line}:${origin.column}, ${origin.startOffset}-${origin.endOffset}]"
  def errorString: String = s"$originString: $message"
}
