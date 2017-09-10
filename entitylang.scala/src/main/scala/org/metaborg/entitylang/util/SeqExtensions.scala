package org.metaborg.entitylang.util

class SeqExtensions[T](val seq: Seq[T]) extends AnyVal {
  def reorderCyclic(pred: T => Boolean): Option[Seq[T]] = {
    var res = Seq.empty[T]
    var remainder = seq
    var found = false
    while(!found){
      if(remainder.isEmpty){
        return None
      }
      found = pred(remainder.head)
      if(!found){
        res = remainder.head +: res
        remainder = remainder.tail
      }
    }
    Some(remainder ++ res)
  }
}
