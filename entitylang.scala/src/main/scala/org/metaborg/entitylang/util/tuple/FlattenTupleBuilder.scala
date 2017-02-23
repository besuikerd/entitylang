package org.metaborg.entitylang.util.tuple

class FlattenTupleBuilder extends AnyRef{
  def apply[T0, T1](tpl0: (T0, T1)): (T0, T1) =   {

    val (tpl1, t0) = tpl0
    (tpl1, t0)
  }
  def apply[T0, T1, T2](tpl0: ((T0, T1), T2)): (T0, T1, T2) =   {

    val (tpl1, t1) = tpl0
    val (tpl2, t0) = tpl1
    (tpl2, t0, t1)
  }
  def apply[T0, T1, T2, T3](tpl0: (((T0, T1), T2), T3)): (T0, T1, T2, T3) =   {

    val (tpl1, t2) = tpl0
    val (tpl2, t1) = tpl1
    val (tpl3, t0) = tpl2
    (tpl3, t0, t1, t2)
  }
  def apply[T0, T1, T2, T3, T4](tpl0: ((((T0, T1), T2), T3), T4)): (T0, T1, T2, T3, T4) =   {

    val (tpl1, t3) = tpl0
    val (tpl2, t2) = tpl1
    val (tpl3, t1) = tpl2
    val (tpl4, t0) = tpl3
    (tpl4, t0, t1, t2, t3)
  }
  def apply[T0, T1, T2, T3, T4, T5](tpl0: (((((T0, T1), T2), T3), T4), T5)): (T0, T1, T2, T3, T4, T5) =   {

    val (tpl1, t4) = tpl0
    val (tpl2, t3) = tpl1
    val (tpl3, t2) = tpl2
    val (tpl4, t1) = tpl3
    val (tpl5, t0) = tpl4
    (tpl5, t0, t1, t2, t3, t4)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6](tpl0: ((((((T0, T1), T2), T3), T4), T5), T6)): (T0, T1, T2, T3, T4, T5, T6) =   {

    val (tpl1, t5) = tpl0
    val (tpl2, t4) = tpl1
    val (tpl3, t3) = tpl2
    val (tpl4, t2) = tpl3
    val (tpl5, t1) = tpl4
    val (tpl6, t0) = tpl5
    (tpl6, t0, t1, t2, t3, t4, t5)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7](tpl0: (((((((T0, T1), T2), T3), T4), T5), T6), T7)): (T0, T1, T2, T3, T4, T5, T6, T7) =   {

    val (tpl1, t6) = tpl0
    val (tpl2, t5) = tpl1
    val (tpl3, t4) = tpl2
    val (tpl4, t3) = tpl3
    val (tpl5, t2) = tpl4
    val (tpl6, t1) = tpl5
    val (tpl7, t0) = tpl6
    (tpl7, t0, t1, t2, t3, t4, t5, t6)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8](tpl0: ((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8)): (T0, T1, T2, T3, T4, T5, T6, T7, T8) =   {

    val (tpl1, t7) = tpl0
    val (tpl2, t6) = tpl1
    val (tpl3, t5) = tpl2
    val (tpl4, t4) = tpl3
    val (tpl5, t3) = tpl4
    val (tpl6, t2) = tpl5
    val (tpl7, t1) = tpl6
    val (tpl8, t0) = tpl7
    (tpl8, t0, t1, t2, t3, t4, t5, t6, t7)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9](tpl0: (((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9) =   {

    val (tpl1, t8) = tpl0
    val (tpl2, t7) = tpl1
    val (tpl3, t6) = tpl2
    val (tpl4, t5) = tpl3
    val (tpl5, t4) = tpl4
    val (tpl6, t3) = tpl5
    val (tpl7, t2) = tpl6
    val (tpl8, t1) = tpl7
    val (tpl9, t0) = tpl8
    (tpl9, t0, t1, t2, t3, t4, t5, t6, t7, t8)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](tpl0: ((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) =   {

    val (tpl1, t9) = tpl0
    val (tpl2, t8) = tpl1
    val (tpl3, t7) = tpl2
    val (tpl4, t6) = tpl3
    val (tpl5, t5) = tpl4
    val (tpl6, t4) = tpl5
    val (tpl7, t3) = tpl6
    val (tpl8, t2) = tpl7
    val (tpl9, t1) = tpl8
    val (tpl10, t0) = tpl9
    (tpl10, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](tpl0: (((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) =   {

    val (tpl1, t10) = tpl0
    val (tpl2, t9) = tpl1
    val (tpl3, t8) = tpl2
    val (tpl4, t7) = tpl3
    val (tpl5, t6) = tpl4
    val (tpl6, t5) = tpl5
    val (tpl7, t4) = tpl6
    val (tpl8, t3) = tpl7
    val (tpl9, t2) = tpl8
    val (tpl10, t1) = tpl9
    val (tpl11, t0) = tpl10
    (tpl11, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](tpl0: ((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) =   {

    val (tpl1, t11) = tpl0
    val (tpl2, t10) = tpl1
    val (tpl3, t9) = tpl2
    val (tpl4, t8) = tpl3
    val (tpl5, t7) = tpl4
    val (tpl6, t6) = tpl5
    val (tpl7, t5) = tpl6
    val (tpl8, t4) = tpl7
    val (tpl9, t3) = tpl8
    val (tpl10, t2) = tpl9
    val (tpl11, t1) = tpl10
    val (tpl12, t0) = tpl11
    (tpl12, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](tpl0: (((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) =   {

    val (tpl1, t12) = tpl0
    val (tpl2, t11) = tpl1
    val (tpl3, t10) = tpl2
    val (tpl4, t9) = tpl3
    val (tpl5, t8) = tpl4
    val (tpl6, t7) = tpl5
    val (tpl7, t6) = tpl6
    val (tpl8, t5) = tpl7
    val (tpl9, t4) = tpl8
    val (tpl10, t3) = tpl9
    val (tpl11, t2) = tpl10
    val (tpl12, t1) = tpl11
    val (tpl13, t0) = tpl12
    (tpl13, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](tpl0: ((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) =   {

    val (tpl1, t13) = tpl0
    val (tpl2, t12) = tpl1
    val (tpl3, t11) = tpl2
    val (tpl4, t10) = tpl3
    val (tpl5, t9) = tpl4
    val (tpl6, t8) = tpl5
    val (tpl7, t7) = tpl6
    val (tpl8, t6) = tpl7
    val (tpl9, t5) = tpl8
    val (tpl10, t4) = tpl9
    val (tpl11, t3) = tpl10
    val (tpl12, t2) = tpl11
    val (tpl13, t1) = tpl12
    val (tpl14, t0) = tpl13
    (tpl14, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](tpl0: (((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14), T15)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) =   {

    val (tpl1, t14) = tpl0
    val (tpl2, t13) = tpl1
    val (tpl3, t12) = tpl2
    val (tpl4, t11) = tpl3
    val (tpl5, t10) = tpl4
    val (tpl6, t9) = tpl5
    val (tpl7, t8) = tpl6
    val (tpl8, t7) = tpl7
    val (tpl9, t6) = tpl8
    val (tpl10, t5) = tpl9
    val (tpl11, t4) = tpl10
    val (tpl12, t3) = tpl11
    val (tpl13, t2) = tpl12
    val (tpl14, t1) = tpl13
    val (tpl15, t0) = tpl14
    (tpl15, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](tpl0: ((((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14), T15), T16)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) =   {

    val (tpl1, t15) = tpl0
    val (tpl2, t14) = tpl1
    val (tpl3, t13) = tpl2
    val (tpl4, t12) = tpl3
    val (tpl5, t11) = tpl4
    val (tpl6, t10) = tpl5
    val (tpl7, t9) = tpl6
    val (tpl8, t8) = tpl7
    val (tpl9, t7) = tpl8
    val (tpl10, t6) = tpl9
    val (tpl11, t5) = tpl10
    val (tpl12, t4) = tpl11
    val (tpl13, t3) = tpl12
    val (tpl14, t2) = tpl13
    val (tpl15, t1) = tpl14
    val (tpl16, t0) = tpl15
    (tpl16, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](tpl0: (((((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14), T15), T16), T17)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) =   {

    val (tpl1, t16) = tpl0
    val (tpl2, t15) = tpl1
    val (tpl3, t14) = tpl2
    val (tpl4, t13) = tpl3
    val (tpl5, t12) = tpl4
    val (tpl6, t11) = tpl5
    val (tpl7, t10) = tpl6
    val (tpl8, t9) = tpl7
    val (tpl9, t8) = tpl8
    val (tpl10, t7) = tpl9
    val (tpl11, t6) = tpl10
    val (tpl12, t5) = tpl11
    val (tpl13, t4) = tpl12
    val (tpl14, t3) = tpl13
    val (tpl15, t2) = tpl14
    val (tpl16, t1) = tpl15
    val (tpl17, t0) = tpl16
    (tpl17, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](tpl0: ((((((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14), T15), T16), T17), T18)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) =   {

    val (tpl1, t17) = tpl0
    val (tpl2, t16) = tpl1
    val (tpl3, t15) = tpl2
    val (tpl4, t14) = tpl3
    val (tpl5, t13) = tpl4
    val (tpl6, t12) = tpl5
    val (tpl7, t11) = tpl6
    val (tpl8, t10) = tpl7
    val (tpl9, t9) = tpl8
    val (tpl10, t8) = tpl9
    val (tpl11, t7) = tpl10
    val (tpl12, t6) = tpl11
    val (tpl13, t5) = tpl12
    val (tpl14, t4) = tpl13
    val (tpl15, t3) = tpl14
    val (tpl16, t2) = tpl15
    val (tpl17, t1) = tpl16
    val (tpl18, t0) = tpl17
    (tpl18, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](tpl0: (((((((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14), T15), T16), T17), T18), T19)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) =   {

    val (tpl1, t18) = tpl0
    val (tpl2, t17) = tpl1
    val (tpl3, t16) = tpl2
    val (tpl4, t15) = tpl3
    val (tpl5, t14) = tpl4
    val (tpl6, t13) = tpl5
    val (tpl7, t12) = tpl6
    val (tpl8, t11) = tpl7
    val (tpl9, t10) = tpl8
    val (tpl10, t9) = tpl9
    val (tpl11, t8) = tpl10
    val (tpl12, t7) = tpl11
    val (tpl13, t6) = tpl12
    val (tpl14, t5) = tpl13
    val (tpl15, t4) = tpl14
    val (tpl16, t3) = tpl15
    val (tpl17, t2) = tpl16
    val (tpl18, t1) = tpl17
    val (tpl19, t0) = tpl18
    (tpl19, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](tpl0: ((((((((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14), T15), T16), T17), T18), T19), T20)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) =   {

    val (tpl1, t19) = tpl0
    val (tpl2, t18) = tpl1
    val (tpl3, t17) = tpl2
    val (tpl4, t16) = tpl3
    val (tpl5, t15) = tpl4
    val (tpl6, t14) = tpl5
    val (tpl7, t13) = tpl6
    val (tpl8, t12) = tpl7
    val (tpl9, t11) = tpl8
    val (tpl10, t10) = tpl9
    val (tpl11, t9) = tpl10
    val (tpl12, t8) = tpl11
    val (tpl13, t7) = tpl12
    val (tpl14, t6) = tpl13
    val (tpl15, t5) = tpl14
    val (tpl16, t4) = tpl15
    val (tpl17, t3) = tpl16
    val (tpl18, t2) = tpl17
    val (tpl19, t1) = tpl18
    val (tpl20, t0) = tpl19
    (tpl20, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
  }
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](tpl0: (((((((((((((((((((((T0, T1), T2), T3), T4), T5), T6), T7), T8), T9), T10), T11), T12), T13), T14), T15), T16), T17), T18), T19), T20), T21)): (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) =   {

    val (tpl1, t20) = tpl0
    val (tpl2, t19) = tpl1
    val (tpl3, t18) = tpl2
    val (tpl4, t17) = tpl3
    val (tpl5, t16) = tpl4
    val (tpl6, t15) = tpl5
    val (tpl7, t14) = tpl6
    val (tpl8, t13) = tpl7
    val (tpl9, t12) = tpl8
    val (tpl10, t11) = tpl9
    val (tpl11, t10) = tpl10
    val (tpl12, t9) = tpl11
    val (tpl13, t8) = tpl12
    val (tpl14, t7) = tpl13
    val (tpl15, t6) = tpl14
    val (tpl16, t5) = tpl15
    val (tpl17, t4) = tpl16
    val (tpl18, t3) = tpl17
    val (tpl19, t2) = tpl18
    val (tpl20, t1) = tpl19
    val (tpl21, t0) = tpl20
    (tpl21, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
  }
}