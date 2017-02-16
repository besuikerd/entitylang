package org.metaborg.entitylang.lang

import org.metaborg.scalaterms.Origin

class RichOrigin(val origin: Origin) extends AnyVal {
  override def toString: String = s"""Origin("${origin.filename}", ${origin.line}:${origin.column}, ${origin.startOffset}-${origin.endOffset})"""
}