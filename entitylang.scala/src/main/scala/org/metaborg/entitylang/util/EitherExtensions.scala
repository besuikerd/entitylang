package org.metaborg.entitylang.util

import org.metaborg.entitylang.util.monoid.Monoid

class EitherExtensions[L, R](val e: Either[L, R]) extends AnyVal {


  def merge[R2](e2: Either[L, R2])(implicit monoid: Monoid[L]): Either[L, (R, R2)] = {
    e match{
      case l @ Left(l1) => e2 match{
        case Left(l2) => Left(monoid.append(l1, l2))
        case otherwise => Left(l1)
      }
      case otherwise => otherwise.right.flatMap{a => e2.right.map(b => (a,b))}
    }
  }
}
