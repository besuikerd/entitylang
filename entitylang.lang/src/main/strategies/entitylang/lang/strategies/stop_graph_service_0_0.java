package entitylang.lang.strategies;

//import org.metaborg.entitylang.graph.webservice.GraphWebService;
import org.spoofax.interpreter.terms.IStrategoTerm;
import org.strategoxt.lang.Context;
import org.strategoxt.lang.Strategy;

public class stop_graph_service_0_0 extends Strategy{
    public static final stop_graph_service_0_0 instance = new stop_graph_service_0_0();

    @Override
    public IStrategoTerm invoke(Context context, IStrategoTerm current) {
//        GraphWebService.stopService(context);
        return context.getFactory().makeConstructor("None", 0);
    }
}
