package org.metaborg.entitylang.util

class RichString(val s: String) extends AnyVal {
  def uncapitalize =
    if(s.isEmpty || s.head.isLower) s
    else {
      val chars = s.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }
}
