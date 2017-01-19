package entitylang.lang.strategies;

import org.strategoxt.lang.Strategy;
import org.spoofax.interpreter.terms.IStrategoTerm;
import org.strategoxt.lang.Context;
import org.metaborg.entitylang.EditorServices;

public class editor_resolve_0_0 extends Strategy{
  public static final editor_resolve_0_0 instance = new editor_resolve_0_0();

  @Override
  public IStrategoTerm invoke(Context context, IStrategoTerm current) {
    return EditorServices.editorResolve(context, current);
  }
}
