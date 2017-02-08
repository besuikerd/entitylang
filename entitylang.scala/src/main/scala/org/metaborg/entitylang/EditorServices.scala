package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.{Old_DataflowAnalysis, Old_TypeAnalysis}
import org.metaborg.entitylang.graph.webservice.GraphWebService
import org.metaborg.entitylang.graph.{Edge, Graph, Node}
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.scalaterms.spoofax._
import org.strategoxt.lang.Context

object EditorServices extends EditorServices{

  override def editorHover(focusedStrategyInput: FocusedStrategyInput)(implicit context: Context): Option[HoverResult] =
    Some(HoverResult(s"Hovering: ${focusedStrategyInput.node}"))

  override def editorAnalyze(generalStrategyInput: GeneralStrategyInput)(implicit context: Context): AnalysisResult = {

    Start1.fromSTerm.unapply(generalStrategyInput.ast) match{
      case Some(start) => {
        val defs = Old_DataflowAnalysis.collectDefinitions(start)

        defs.nodes.foreach(e => context.getIOAgent.printError(e.toString))
        context.getIOAgent.printError(defs.toString)


        for(c <- defs.calculations){
          val edges = Old_DataflowAnalysis.dependencyCalculation(defs.nodes)(c)
          context.getIOAgent.printError(
            s"""------------------------
               |${c.attribute}[${edges.size}]
               |------------------------""".stripMargin)
          for(org.metaborg.entitylang.analysis.Old_DataflowAnalysis.Edge(from, to) <- edges) {
            context.getIOAgent.printError(s"$from -> $to")
          }
        }

        val dataflowGraph = Old_DataflowAnalysis.dataflowAnalysis(start)

        val graph = Graph(dataflowGraph.nodes.map(n => Node(n.index.toString)), dataflowGraph.edges.map(e => Edge(e.from.toString, e.to.toString)))
      }
      case None => context.getIOAgent.printError("analysis failed, mismatched ast")
    }


    AnalysisResult(generalStrategyInput.ast, Seq.empty, Seq.empty, Seq.empty)
  }

  override def editorResolve(focusedStrategyInput: FocusedStrategyInput)(implicit context: Context): Option[ResolutionResult] = None
}
