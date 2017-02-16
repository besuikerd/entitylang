package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.Analyzer
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
        val model = Analyzer.analyze(start)
        AnalysisResult(generalStrategyInput.ast, Seq.empty, Seq.empty, Seq.empty)
      }
      case None => context.getIOAgent.printError("analysis failed, mismatched ast")
    }


    AnalysisResult(generalStrategyInput.ast, Seq.empty, Seq.empty, Seq.empty)
  }

  override def editorResolve(focusedStrategyInput: FocusedStrategyInput)(implicit context: Context): Option[ResolutionResult] = None
}
