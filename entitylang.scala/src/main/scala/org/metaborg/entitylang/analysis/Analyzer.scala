package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.{SExp, SLiteral}
import org.metaborg.entitylang.lang.ast.MExpression.SExp._
import org.metaborg.entitylang.lang.ast.MExpression.SLiteral._
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}

import scalax.collection.GraphPredef._

object Analyzer {
  def analyze(ast: Start1): AnalysisModel = {
    val pass1 = collectDefinitions(ast)
    val pass2 = collectDerivedValueDependencies(pass1)
//    val pass3 = deriveTypes(pass2)
    pass2
  }

  def collectDefinitions(ast: Start1): AnalysisModel = {
    ast.model1.value.foldLeft(AnalysisModel.empty) {
      case (model, unit) => unit match {
        case e@Entity2(id1, member2, origin) => {
          val entityNode = EntityNode(id1.string)
          val entityFieldNodeData = EntityNodeData(e)

          model.foldWith(member2.value)(model => {
            case attribute@Attribute2(name, tpe, origin) =>
              val entityFieldNode = AttributeNode(entityNode.name, name.string)
              val entityFieldData = AttributeNodeData(BaseType(tpe), entityFieldNode, attribute)
              model.copy(
                fields = model.fields + (entityFieldNode -> entityFieldData),
                graph = model.graph + (entityNode ~> entityFieldNode)
              )
            case derivedValue@DerivedAttribute3(name, tpe, e, origin) =>
              val entityFieldNode = DerivedValueNode(entityNode.name, name.string)
              val entityFieldNodeData = DerivedValueNodeData(BaseType(tpe), entityFieldNode, derivedValue)
              model.copy(
                fields = model.fields + (entityFieldNode -> entityFieldNodeData),
                graph = model.graph + (entityNode ~> entityFieldNode)
              )
          })
        }
        case relation@Relation6(entityRefLeft: EntityRef1, attributeRefLeft: AttributeRef1, multiplicityLeft, multiplicityRight, entityRefRight: EntityRef1, attributeRefRight: AttributeRef1, _) =>
          val entityNodeLeft = EntityNode(entityRefLeft.id1.string)
          val relationNodeLeft = RelationNode(entityNodeLeft.name, attributeRefLeft.id1.string)
          val relationNodeDataLeft = RelationNodeData(EntityType(entityRefRight.id1.string), entityRefLeft, attributeRefLeft, multiplicityLeft, relation)

          val entityNodeRight = EntityNode(entityRefRight.id1.string)
          val relationNodeRight = RelationNode(entityNodeRight.name, attributeRefRight.id1.string)
          val relationNodeDataRight = RelationNodeData(EntityType(entityRefLeft.id1.string), entityRefRight, attributeRefRight, multiplicityRight, relation)

          model.copy(
            fields = model.fields
              + (relationNodeLeft -> relationNodeDataLeft)
              + (relationNodeRight -> relationNodeDataRight),
            graph = model.graph
              + (entityNodeLeft ~> relationNodeLeft)
              + (entityNodeRight ~> relationNodeRight)
          )
      }
    }
  }

  def collectDerivedValueDependencies(model: AnalysisModel): AnalysisModel = {
    def addDependency(model: AnalysisModel, entityNode: EntityNode, field: String): (EntityNode, Seq[AnalysisNode]) = {
      val optDependency = for {
        entityField <- model.graph.findEntityField(entityNode.name, field)
        entityFieldData <- model.fields.get(entityField.typedValue[EntityFieldNode])
        targetEntity <- Some(entityFieldData.fieldType match {
          case EntityType(t) => t
          case _ => entityNode.name
        })
      } yield (EntityNode(targetEntity), entityField)
      optDependency match {
        case Some((entityNode, target)) => (entityNode, Seq(target))
        case None => (entityNode, Seq.empty)
      }
    }

    def walk(model: AnalysisModel, entityNode: EntityNode, e: SExp): (EntityNode, Seq[AnalysisNode]) = e match {
      case MemberAccess2(e1, name, _) =>
        val (entityNode2, edges) = walk(model, entityNode, e1)
        addDependency(model, entityNode2, name.string)
      case UnExp(_, e1) => walk(model, entityNode, e1)
      case BinExp(_, e1, e2) =>
        val (_, nodes1) = walk(model, entityNode, e1)
        val (_, nodes2) = walk(model, entityNode, e2)
        (entityNode, nodes1 ++ nodes2)
      case Apply2(e1, params, origin) =>
        (entityNode, params.value.flatMap(e => walk(model, entityNode, e)._2))
      case Ref1(id1, origin) =>
        addDependency(model, entityNode, id1.string)
      case _ => (entityNode, Seq.empty)
    }

    model.graph.derivedValues.foldLeft(model) { case (model, node) =>
      val targets = for {
        derivedValueNode <- node.optTypedValue[DerivedValueNode]
        derivedValueData@DerivedValueNodeData(_, _, _) <- model.fields.get(derivedValueNode)
        entityNode <- model.graph.findEntity(derivedValueNode.entity)
      } yield {
        walk(model, entityNode.typedValue[EntityNode], derivedValueData.term.exp3)._2
      }

      val graph2 = targets.toSeq.flatten[AnalysisNode].foldLeft(model.graph) { case (graph, targetNode) => graph + (node.value ~> targetNode.value) }
      model.copy(
        graph = graph2
      )
    }
  }

//  def deriveTypes(model: AnalysisModel): AnalysisModel = {
//    val scss = model.graph.stronglyConnectedComponents.map(n => n.findCycle.map(_.nodes.toList.distinct).getOrElse(Seq(n)))
//
//    scss.foldLeft(model) {
//      case (model, scc) =>
//        if (scc.length == 1)
//    }
//  }
//
//
//  def deriveType(model: AnalysisModel, e: SExp): BaseType = {
//    import org.metaborg.entitylang.analysis.types.TypeDescription._
//    def walk(model: AnalysisModel, e: SExp): (AnalysisModel, BaseType) = {
//      implicit def baseTypeToResult(b: BaseType): (AnalysisModel, BaseType) = (model, b)
//      e match {
//        case literal: SLiteral => literal match {
//          case Int1(int1, origin) => BaseType.int
//          case Float1(float1, origin) => BaseType.float
//          case String1(string1, origin) => BaseType.string
//          case True0(origin) => BaseType.boolean
//          case False0(origin) => BaseType.boolean
//        }
//        case BinExp(op, e1, e2) => {
//          val (t1, t2) = (deriveType(model, e1), deriveType(model, e2))
//          def expectType(expected: BaseType)(actual: BaseType) = ???
//
//          val description = t1.hasType(BaseType.int) && t2.hasType(BaseType.int) ==> BaseType.int
//
//          op match {
//            case Add =>
//            case Sub =>
//            case Mul =>
//            case Div =>
//            case Mod =>
//            case LessThan =>
//            case LessThanEqual =>
//            case GreaterThan =>
//            case GreaterThanEqual =>
//            case Equal =>
//            case Inequal =>
//            case And =>
//            case Or =>
//            case Merge =>
//            case ChoiceLeft =>
//          }
//        }
//
//        case MemberAccess2(e1, field, origin) =>
//        case UnExp(op, e1) =>
//        case Apply2(e1, e2, origin) =>
//        case Ref1(field, origin) =>
//        //    case This0(origin) =>
//      }
//
//
//    }
//    walk(model, e)._2
//  }
}
