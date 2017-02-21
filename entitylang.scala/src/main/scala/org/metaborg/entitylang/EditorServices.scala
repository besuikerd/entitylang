package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.Analyzer
import org.metaborg.entitylang.graph.webservice.GraphWebService
import org.metaborg.entitylang.graph.{Edge, Graph, Node}
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax._
import org.strategoxt.lang.Context

object EditorServices extends EditorServices{

  override def editorHover(focusedStrategyInput: FocusedStrategyInput)(implicit context: Context): Option[HoverResult] =
    focusedStrategyInput.node match{
      case e: SExp => Some(HoverResult(s"Hovering $e"))
      case _ => None
    }

  override def editorAnalyze(generalStrategyInput: GeneralStrategyInput)(implicit context: Context): AnalysisResult = {

    Start1.fromSTerm.unapply(generalStrategyInput.ast) match{
      case Some(start) => {
        val model = Analyzer.analyze(start)
        context.getIOAgent.printError(model.toString)
        AnalysisResult(generalStrategyInput.ast, model.errors.map(e => if(e.origin == null) e.copy(origin = new Origin(generalStrategyInput.path, 0, 0, 0, 0)) else e), Seq.empty, Seq.empty)
      }
      case None => {
        context.getIOAgent.printError("analysis failed, mismatched ast")
        AnalysisResult(generalStrategyInput.ast, Seq.empty, Seq.empty, Seq.empty)
      }
    }
  }

  override def editorResolve(focusedStrategyInput: FocusedStrategyInput)(implicit context: Context): Option[ResolutionResult] = None
}
