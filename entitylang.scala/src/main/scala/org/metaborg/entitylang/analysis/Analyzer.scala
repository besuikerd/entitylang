package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.{ExpressionTypeSystem, MultiplicityTypeSystem, OptionalTypeTypeSystem, PrimitiveTypeWithMultiplicityTypeSystem}
import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, TypeError}
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.generate.entity.invalidation.js.invalidation
import org.metaborg.entitylang.lang.ast.MCommon.SID
import org.metaborg.entitylang.lang.ast.MExpression.{SExp, SLiteral}
import org.metaborg.entitylang.lang.ast.MExpression.SExp._
import org.metaborg.entitylang.lang.ast.MExpression.SLambdaParameter.LambdaParameter2
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.MType.SType.EntityType1
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.entitylang.util.profiler.Profiler
import org.slf4j.LoggerFactory
import org.metaborg.entitylang.util._

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LBase.LEdgeImplicits

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
    val pass4 = calculateDerivedValueInvalidations(pass3)
    if(pass4.errors.nonEmpty){
      logger.warn("found the following errors:")
      pass4.errors.sortWith((e1, e2) => e1.origin.line < e2.origin.line || e1.origin.line == e2.origin.line && e1.origin.column < e2.origin.column).foreach(e => logger.warn(GeneralTypeError(e.origin, e.message).errorString))
    }
    logger.info(s"calculateDerivedValueInvalidations took ${profiler.getAndTick()}ms")
    logger.info(s"type checker took ${profiler.total()}ms" )
    logger.info(pass4.graph.toString())
    pass4
  }

  def collectDefinitions(ast: Start1): AnalysisModel = {
    ast.model1.value.foldLeft(AnalysisModel.empty) {
      case (model, unit) => unit match {
        case Entity2(SID(entityName, _), member2, origin) => {
          val entityFields: Seq[EntityFieldNodeData] = member2.value.map {
            case attribute @ Attribute2(SID(fieldName, _), tpe, origin) =>
              val entityFieldNode = EntityFieldNode(entityName, fieldName)
              val inferredType = PrimitiveTypeWithMultiplicityTypeSystem.infer(tpe).right.getOrElse(top)
              AttributeNodeData(inferredType, entityFieldNode, attribute)
            case derivedValue @ DerivedAttribute3(SID(fieldName, _), tpe, e, origin) =>
              val entityFieldNode = EntityFieldNode(entityName, fieldName)
              val inferredType = OptionalTypeTypeSystem.infer(tpe).right.getOrElse(top)
              DerivedValueNodeData(inferredType, entityFieldNode, derivedValue)
          }
          model.copy(
            fields = model.fields ++ entityFields.map(f => f.node -> f)
            , graph = model.graph ++ entityFields.map(field => OuterNode(field.node))
          )
        }
        case relation@Relation6(
          entityRefLeft @ EntityRef1(SID(entityNameLeft, _), _),
          attributeRefLeft @ AttributeRef1(SID(attributeNameLeft, _), _),
          multiplicityLeft,
          multiplicityRight,
          entityRefRight @ EntityRef1(SID(entityNameRight, _), _),
          attributeRefRight @ AttributeRef1(SID(attributeNameRight, _), _),
          _
        ) =>
          val relationNodeLeft = EntityFieldNode(entityNameLeft, attributeNameLeft)
          val relationNodeRight = EntityFieldNode(entityNameRight, attributeNameRight)

          val leftType = MultiplicityTypeSystem.infer(multiplicityLeft).right.map(m => MultiplicityType(EntityType(entityRefRight.id1.string), m)).right.get
          val relationNodeDataLeft = RelationNodeData(leftType, entityRefLeft, attributeRefLeft, multiplicityLeft, relationNodeLeft, relationNodeRight, relation)


          val rightType = MultiplicityTypeSystem.infer(multiplicityRight).right.map(m => MultiplicityType(EntityType(entityRefLeft.id1.string), m)).right.get
          val relationNodeDataRight = RelationNodeData(rightType, entityRefRight, attributeRefRight, multiplicityRight, relationNodeRight, relationNodeLeft, relation)

          model
            .verifyUniqueField(relationNodeDataLeft)
            .verifyUniqueField(relationNodeDataRight)
            .copy(
            fields = model.fields
              + (relationNodeLeft -> relationNodeDataLeft)
              + (relationNodeRight -> relationNodeDataRight),
            graph = model.graph
              + relationNodeLeft
              + relationNodeRight
          )
      }
    }
  }


  def collectDerivedValueDependencies(model: AnalysisModel): AnalysisModel = {
    type Path = Seq[EntityFieldNode]
    case class WalkContext(entity: String, path: Path, bindings: Map[String, EntityFieldNode] = Map.empty)

    def addDependency(model: AnalysisModel, context: WalkContext, field: String): (WalkContext, Seq[Path]) = {
      context.bindings.get(field) match{
        case Some(targetNode) =>
          val path = targetNode +: context.path
          (context.copy(
            entity = targetNode.entity,
            path = path
          ), Seq(path))
        case None =>
          val targetNode = EntityFieldNode(context.entity, field)
          val optDependency = for {
            entityFieldData <- model.fields.get(targetNode)
            targetEntity = entityFieldData.fieldType match {
              case MultiplicityType(EntityType(t), _) => t
              case _ => context.entity
            }
          } yield context.copy(
            entity = targetEntity
            , path = targetNode +: context.path
          )
          optDependency match{
            case Some(ctx) => (ctx, Seq(ctx.path))
            case None => (context, Seq.empty)
          }
      }
//      context.bindings.get(field) match{
//        case Some(targetNode) =>
//          val context2 = context.copy(
//            entity = targetNode.entity
//            , path = EntityFieldNode(context.entity, field) +: context.path
//          )
//          (context2, Seq.empty)
//        case None => //in case we refer to a field in that node, add an edge
//          val optDependency = for {
//            entityField <- model.graph.findEntityField(context.entity, field)
//            entityFieldData <- model.fields.get(entityField.typedValue[EntityFieldNode])
//            targetEntity <- Some(entityFieldData.fieldType match {
//              case MultiplicityType(EntityType(t), _) => t
//              case _ => context.entity
//            })
//          } yield (context.copy(entity = targetEntity), entityField)
//          optDependency match {
//            case Some((context2, target)) => (context2, Seq(target.value))
//            case None => (context, Seq.empty)
//          }
//      }
    }

    def walk(model: AnalysisModel, context: WalkContext, e: SExp): (WalkContext, Seq[Path]) = e match {
      case MemberAccess2(e1, name, _) =>
        val (context2, edges) = walk(model, context, e1)
        val (context3, edges2) = addDependency(model, context2, name.string)
        (context3, edges ++ edges2)
      case UnExp(_, e1) => walk(model, context, e1)
      case BinExp(_, e1, e2) =>
        val (_, nodes1) = walk(model, context, e1)
        val (_, nodes2) = walk(model, context, e2)
        (context, nodes1 ++ nodes2)
      case Apply2(e1, params, origin) =>
        (context, params.value.flatMap(e => walk(model, context, e)._2))
      case Ref1(id1, origin) =>
        addDependency(model, context, id1.string)
      case If3(e1, e2, e3, origin) =>
        val (_, nodes1) = walk(model, context, e1)
        val (_, nodes2) = walk(model, context, e2)
        val (_, nodes3) = walk(model, context, e3)
        (context, nodes1 ++ nodes2 ++ nodes3)
      case literal: SLiteral => (context, Seq.empty)
      case Lambda2(parameters, e, _) => //If first parameter is a lambda, bind last node of path to the parameter
        val entityParameters = parameters.value.headOption.collect{
          case LambdaParameter2(id, t: EntityType1, _) if context.path.nonEmpty && t.id1.string == context.path.head.entity =>
            id.string -> context.path.head
        }.toMap
        val context2 = context.copy(bindings =  context.bindings ++ entityParameters)
        (context, walk(model, context2, e)._2)
      case MethodCall3(e1, _, params, _) =>
        val (context2, nodes1) = walk(model, context, e1)
        (context, params.value.foldLeft(nodes1){case (acc, cur) => acc ++ walk(model, context2, cur)._2})
    }

    val edges: Seq[AnalysisEdge] = model.derivedValues.flatMap{ nodeData =>
      val (_, paths) = walk(model, WalkContext(nodeData.node.entity, Seq.empty, Map.empty), nodeData.term.exp3)
      paths.map{case target +: pathTo => (nodeData.node ~+#> target)(pathTo)}
    }

    model.copy(graph = model.graph ++ edges)

//      val graph2 = targets.toSeq.flatten[AnalysisNode].foldLeft(model.graph) { case (graph, targetNode) => graph + (node.value ~> targetNode.value) }
//      model.copy(
//        graph = graph2
//      )

  }

  def deriveTypes(model: AnalysisModel): AnalysisModel = {
//    val scss = model.graph.stronglyConnectedComponents

    val scss = mutableSCC(model.graph)

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

  case class FieldWithMultiplicity(
    field: EntityFieldNode,
    multiplicity: MultiplicityBounds
  )
  case class PathField(
    field: EntityFieldNode,
    previousMultiplicity: MultiplicityBounds,
    nextMultiplicity: MultiplicityBounds
  )
  type InvalidationPath = Seq[PathField]
  case class FieldInvalidation(
    field: FieldWithMultiplicity,
    path: InvalidationPath
  )
  case class InvalidationFunction(field: EntityFieldNode, invalidations: Seq[FieldInvalidation], isDerivedValue: Boolean)
  type Invalidations = Seq[InvalidationFunction]

  def calculateDerivedValueInvalidations(model: AnalysisModel): AnalysisModel = {

    object EdgeLabelImplicits extends LEdgeImplicits[List[EntityFieldNode]]
    import EdgeLabelImplicits._

    val invalidationFunctions: Invalidations =
      for(field <- model.fields.values.toList) yield {
        val node = model.graph.get(field.node)
        logger.info(s"invalidating ${node.value}")
        val paths: Seq[FieldInvalidation] = for(edge <- node.incoming.toSeq) yield {
          val (multiplicity, path) = edge.label.foldLeft[(MultiplicityBounds, InvalidationPath)]((MultiplicityBounds.oneToOne, Seq.empty)){
            case ((multiplicity, seq), relation) => {
              val inverse = model.fields(relation).asInstanceOf[RelationNodeData].inverse
              val inverseMultiplicity = model.fields(inverse).asInstanceOf[RelationNodeData].fieldType.multiplicity

              val lubMultiplicity = MultiplicityBounds.lub(multiplicity, inverseMultiplicity)
              (lubMultiplicity, seq :+ PathField(inverse, multiplicity, inverseMultiplicity))
            }
          }
          FieldInvalidation(FieldWithMultiplicity(edge.from.value, multiplicity), path)
        }
        InvalidationFunction(field.node, paths, field.isInstanceOf[DerivedValueNodeData])
      }
    invalidationFunctions.map(invalidation.apply).foreach(println)
    model
  }
}
