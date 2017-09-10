@import org.metaborg.entitylang.lang.ast.MExpression.SExp
@import org.metaborg.entitylang.lang.ast.MExpression.SLiteral
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.MemberAccess2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.MethodCall3
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Lambda2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Not1
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Multiplication2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Division2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Modulo2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Addition2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Subtraction2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.LessThan2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.LessThanEqual2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.GreaterThan2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.GreaterThanEqual2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Equal2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Inequal2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.And2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Or2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Merge2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.ChoiceLeft2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Apply2
@import org.metaborg.entitylang.lang.ast.MExpression.SExp.Ref1

@import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.Int1
@import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.Float1
@import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.String1
@import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.True0
@import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.False0
@import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.Null0
@(e: SExp) @exp(e)

@exp(e: SExp) = @{e match{
  case l: SLiteral => literal(l)
  case MemberAccess2(exp1, id2, origin) =>
  case MethodCall3(exp1, id2, exp3, origin) =>
  case Lambda2(lambdaparameter1, exp2, origin) =>
  case Not1(exp1, origin) =>
  case Multiplication2(exp1, exp2, origin) =>
  case Division2(exp1, exp2, origin) =>
  case Modulo2(exp1, exp2, origin) =>
  case Addition2(exp1, exp2, origin) =>
  case Subtraction2(exp1, exp2, origin) =>
  case LessThan2(exp1, exp2, origin) =>
  case LessThanEqual2(exp1, exp2, origin) =>
  case GreaterThan2(exp1, exp2, origin) =>
  case GreaterThanEqual2(exp1, exp2, origin) =>
  case Equal2(exp1, exp2, origin) =>
  case Inequal2(exp1, exp2, origin) =>
  case And2(exp1, exp2, origin) =>
  case Or2(exp1, exp2, origin) =>
  case If3(exp1, exp2, exp3, origin) =>
  case Merge2(exp1, exp2, origin) =>
  case ChoiceLeft2(exp1, exp2, origin) =>
  case Apply2(exp1, exp2, origin) =>
  case Ref1(id1, origin) =>
}}

@literal(l: SLiteral) = @{l match {
  case Int1(int1, origin) => int1.string
  case Float1(float1, origin) => float1.string
  case String1(string1, origin) => string1.string
  case True0(origin) => "true"
  case False0(origin) => "false"
  case Null0(origin) => "null"
}}