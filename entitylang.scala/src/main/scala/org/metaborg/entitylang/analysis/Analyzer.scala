package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.{ExpressionTypeSystem, MultiplicityTypeSystem, OptionalTypeTypeSystem, PrimitiveTypeWithMultiplicityTypeSystem}
import org.metaborg.entitylang.analysis.types.typesystem.error.TypeError
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp._
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.entitylang.util.seqExtensions

import scalax.collection.GraphPredef._

object Analyzer {
  def analyze(ast: Start1): AnalysisModel = {
    val pass1 = collectDefinitions(ast)
    val pass2 = collectDerivedValueDependencies(pass1)
    val pass3 = deriveTypes(pass2)
    pass3
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
              val inferredType = PrimitiveTypeWithMultiplicityTypeSystem.infer(tpe).right.getOrElse(top)
              val entityFieldData = AttributeNodeData(inferredType, entityFieldNode, attribute)
              model.copy(
                fields = model.fields + (entityFieldNode -> entityFieldData),
                graph = model.graph + (entityNode ~> entityFieldNode)
              )
            case derivedValue@DerivedAttribute3(name, tpe, e, origin) =>
              val entityFieldNode = DerivedValueNode(entityNode.name, name.string)
              val inferredType = OptionalTypeTypeSystem.infer(tpe).right.getOrElse(top)
              val entityFieldNodeData = DerivedValueNodeData(inferredType, entityFieldNode, derivedValue)
              model.copy(
                fields = model.fields + (entityFieldNode -> entityFieldNodeData),
                graph = model.graph + (entityNode ~> entityFieldNode)
              )
          })
        }
        case relation@Relation6(entityRefLeft: EntityRef1, attributeRefLeft: AttributeRef1, multiplicityLeft, multiplicityRight, entityRefRight: EntityRef1, attributeRefRight: AttributeRef1, _) =>
          val entityNodeLeft = EntityNode(entityRefLeft.id1.string)
          val relationNodeLeft = RelationNode(entityNodeLeft.name, attributeRefLeft.id1.string)
          val leftType = MultiplicityTypeSystem.infer(multiplicityLeft).right.map(m => MultiplicityType(EntityType(entityRefRight.id1.string), m)).right.get
          val relationNodeDataLeft = RelationNodeData(leftType, entityRefLeft, attributeRefLeft, multiplicityLeft, relationNodeLeft, relation)

          val entityNodeRight = EntityNode(entityRefRight.id1.string)
          val relationNodeRight = RelationNode(entityNodeRight.name, attributeRefRight.id1.string)
          val rightType = MultiplicityTypeSystem.infer(multiplicityRight).right.map(m => MultiplicityType(EntityType(entityRefLeft.id1.string), m)).right.get
          val relationNodeDataRight = RelationNodeData(rightType, entityRefRight, attributeRefRight, multiplicityRight, relationNodeRight, relation)

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
          case MultiplicityType(EntityType(t), _) => t
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
        val (entityNode3, edges2) = addDependency(model, entityNode2, name.string)
        (entityNode3, edges ++ edges2)
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

  def deriveTypes(model: AnalysisModel): AnalysisModel = {
    val scss = model.graph.stronglyConnectedComponents
    scss.foldLeft(model) {
      case (model, scc) => scc match{
        case Left(cycle) => {
          //arrange the cycle so that a known type is at the head of the cycle
          val ordered = cycle.reorderCyclic { node =>
            val d = model.fields(node.value.asInstanceOf[EntityFieldNode])
            d.fieldType != TopType()
          }

          ordered match{
            case Some(order) =>
              order.foldLeft(model)(inferFieldType)

            //no order found; types cannot be inferred
            case None => cycle.foldLeft(model){
              case (model, field) =>
                val fieldData = model.fields(field.value.asInstanceOf[EntityFieldNode])
                model.reportError("Cycle in found in derived values; type could not be inferred", fieldData.term.origin)
            }
          }
        }

        case Right(node) => inferFieldType(model, node)
      }
    }
  }

  def inferFieldType(model: AnalysisModel, node: AnalysisNode): AnalysisModel = {
    model.fields(node.value.asInstanceOf[EntityFieldNode]) match{
      case d @ DerivedValueNodeData(tpe, node, term) => {
        val scope = model.entityScope(node.entity) ++ model.fields.map{
          case (field, data) => s"${field.entity}.${field.name}" -> data.fieldType
        }
        val typeSystem = ExpressionTypeSystem.typeSystem.withBindings(scope)
        val inferred = typeSystem.infer(term.exp3)
        inferred match {
          case Left(TypeError(origin, message)) =>
            model.reportError(message, origin)
          case Right(inferredType) => {
            println(s"${node.entity}.${node.name} : $inferredType")
            tpe match{
              case TopType() => model.copy(fields = model.fields +
                (node -> d.copy(fieldType = inferredType))
              )
              case t if inferredType != t => model.reportError(s"Expected type: $t, got: $inferredType", term.exp3.origin)
              case t => model
            }
          }
        }
      }
      case _ => model
    }
  }
}
