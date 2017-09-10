package org.metaborg.entitylang

import org.metaborg.scalaterms.Origin

package object lang {
  @inline implicit def richOrigin(origin: Origin): RichOrigin = new RichOrigin(origin)
}
