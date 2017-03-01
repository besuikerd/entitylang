package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.{ExpressionTypeSystem, MultiplicityTypeSystem, OptionalTypeTypeSystem, PrimitiveTypeWithMultiplicityTypeSystem}
import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, TypeError}
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.{SExp, SLiteral}
import org.metaborg.entitylang.lang.ast.MExpression.SExp._
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.entitylang.util.profiler.Profiler
import org.slf4j.LoggerFactory

import scalax.collection.GraphPredef._

object Analyzer {
  val logger = LoggerFactory.getLogger(getClass)

  def analyze(ast: Start1): AnalysisModel = {
    val time = System.currentTimeMillis()

    val profiler = Profiler()
    profiler.start()
    val pass1 = collectDefinitions(ast)
    logger.info(s"collectDefinitions took ${profiler.getAndTick()}ms")
    val pass2 = collectDerivedValueDependencies(pass1)
    logger.info(s"collectDerivedValueDependencies took ${profiler.getAndTick()}ms")
    val pass3 = deriveTypes(pass2)
    logger.info(s"deriveTypes took ${profiler.getAndTick()}ms")
    logger.info(s"type checker took ${profiler.total()}ms" )
    if(pass3.errors.nonEmpty){
      logger.warn("found the following errors:")
      pass3.errors.sortWith((e1, e2) => e1.origin.line < e2.origin.line || e1.origin.line == e2.origin.line && e1.origin.column < e2.origin.column).foreach(e => logger.warn(GeneralTypeError(e.origin, e.message).errorString))
    }
    pass3
  }

  def collectDefinitions(ast: Start1): AnalysisModel = {
    ast.model1.value.foldLeft(AnalysisModel.empty) {
      case (model, unit) => unit match {
        case e@Entity2(id1, member2, origin) => {
          val entityNode = EntityNode(id1.string)
          val entityNodeData = EntityNodeData(e)
          model.copy(entities = model.entities + (entityNode -> entityNodeData)).foldWith(member2.value)(model => member => {
            val entityFieldNodeData = member match {
              case attribute@Attribute2(name, tpe, origin) =>
                val entityFieldNode = AttributeNode(entityNode.name, name.string)
                val inferredType = PrimitiveTypeWithMultiplicityTypeSystem.infer(tpe).right.getOrElse(top)
                AttributeNodeData(inferredType, entityFieldNode, attribute)
              case derivedValue@DerivedAttribute3(name, tpe, e, origin) =>
                val entityFieldNode = DerivedValueNode(entityNode.name, name.string)
                val inferredType = OptionalTypeTypeSystem.infer(tpe).right.getOrElse(top)
                DerivedValueNodeData(inferredType, entityFieldNode, derivedValue)
            }

            model.verifyUniqueField(entityFieldNodeData).copy(
              fields = model.fields + (entityFieldNodeData.node -> entityFieldNodeData),
              graph = model.graph + (entityNode ~> entityFieldNodeData.node)
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

          model
            .verifyUniqueField(relationNodeDataLeft)
            .verifyUniqueField(relationNodeDataRight)
            .copy(
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
      case If3(e1, e2, e3, origin) =>
        val (_, nodes1) = walk(model, entityNode, e1)
        val (_, nodes2) = walk(model, entityNode, e2)
        val (_, nodes3) = walk(model, entityNode, e3)
        (entityNode, nodes1 ++ nodes2 ++ nodes3)
      case literal: SLiteral => (entityNode, Seq.empty)
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
//    val scss = model.graph.stronglyConnectedComponents

    val scss = mutableSCC(model.graph.filter(model.graph.having(node = n => n.value.isInstanceOf[EntityFieldNode])))

    val fieldCount = scss.foldLeft(0){ case (acc, Left(n)) => acc + n.length; case (acc, Right(n)) => acc + 1}
    logger.info(s"scc found $fieldCount fields out of ${model.fields.size}")
    logger.info("Fields will be resolved in the following order")
    logger.info("----------------------------------------------")
    scss.foreach(_.fold(
      cycle => {
        logger.info("Cycle(")
        cycle.map(_.value.asInstanceOf[EntityFieldNode]).foreach(n => logger.info(s"  ${n.entity}.${n.name}"))
        logger.info(")")
      },
      node => {
        val value = node.value.asInstanceOf[EntityFieldNode]
        logger.info(s"${value.entity}.${value.name}")
      }
    ))
    logger.info("----------------------------------------------")

    scss.foldLeft(model) {
      case (model, scc) => scc match{
        case Left(cycle) => {


          val nodeData = cycle.map(n => model.fields(n.value.asInstanceOf[EntityFieldNode]))
          val (typed, untyped) = nodeData.partition(_.fieldType != top)

          def resolve(model: AnalysisModel, typed: Seq[EntityFieldNodeData], untyped: Seq[EntityFieldNodeData]): AnalysisModel = {
            if(untyped.isEmpty){
              model
            } else{
              //find untyped nodes that only have dependencies on typed nodes
              val (typeNow, typeLater) = untyped.partition(
                n => model.graph.get(n.node).outgoing.map(_.to.value.asInstanceOf[EntityFieldNode]).forall(n => !untyped.exists(n2 => n2.node == n))
              )

              logger.info(s"resolving this round: ${typeNow.map(n => s"${n.node.entity}.${n.node.name}").mkString("(", ", ", ")")}" )

              if(typeNow.isEmpty && typeLater.nonEmpty){
                typeLater.foldLeft(model){ case (model, n) => model.reportError(s"Could not resolve cyclic dependency", n.term.origin)}
              } else{
                val model2 = typeNow.foldLeft(model)(inferFieldType)
                resolve(model2, typed ++ typeNow, typeLater)
              }
            }
          }
          val model2 = resolve(model, typed, untyped)
          typed.foldLeft(model2)(inferFieldType)
        }
        case Right(node) => inferFieldType(model, node)
      }
    }
  }

  def inferFieldType(model: AnalysisModel, node: AnalysisNode): AnalysisModel = {
    inferFieldType(model, model.fields(node.value.asInstanceOf[EntityFieldNode]))
  }

  def inferFieldType(model: AnalysisModel, node: EntityFieldNodeData): AnalysisModel = node match {
    case d @ DerivedValueNodeData(tpe, node, term) => {
      val scope = model.entityScope(node.entity) ++ model.fields.map{
        case (field, data) => s"${field.entity}.${field.name}" -> data.fieldType
      }
      val typeSystem = ExpressionTypeSystem.withBindings(scope)
      val inferred = typeSystem.infer(term.exp3)
      inferred match {
        case Left(errors) =>
          model.foldWith(errors)(model => error => model.reportError(error.message, error.origin))
        case Right(inferredType) => {
          println(s"${node.entity}.${node.name} : ${Type.ppType(inferredType)}")
          tpe match{
            case TopType() => model.copy(fields = model.fields +
              (node -> d.copy(fieldType = inferredType))
            )
            case t1 @ MultiplicityType(baseType, m) =>
              inferredType match{
                case t2 @ MultiplicityType(baseType2, m2) =>
                  if(baseType == baseType2 || BaseType.partialOrdering.gteq(baseType, baseType2)){
                    if(m >= m2){
                      model
                    } else{
                      model.reportError(s"Multiplicities are not compatible: $m <-> $m2", term.exp3.origin)
                    }
                  } else{
                    model.reportError(s"Expected base type ${Type.ppBaseType(baseType)}, got: ${Type.ppBaseType(baseType2)}", term.exp3.origin)
                  }
                case otherwise => model.reportError(s"Expected type with multiplicity, got: ${Type.ppType(otherwise)}", term.exp3.origin)
              }
            case t => model.reportError("Field type should be a type with multiplicity", term.optionaltype2.origin)
          }
        }
      }
    }
    case _ => model
  }
}
