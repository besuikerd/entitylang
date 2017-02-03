package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.desugar.{BinExp, UnExp}
import org.metaborg.entitylang.lang.ast.MExpression.SExp._
import org.metaborg.entitylang.lang.ast.MExpression.{SExp, SLiteral}
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.MType.SMultiplicity.{One0, OneOrMore0, ZeroOrMore0, ZeroOrOne0}
import org.metaborg.entitylang.lang.ast.MModel.SMember
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveType.{Boolean0, Float0, Int0, String0}
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveTypeWithMultiplicity.{PrimitiveTypeWithDefaultMultiplicity1, PrimitiveTypeWithMultiplicity2}
import org.metaborg.entitylang.lang.ast.MType.{SMultiplicity, SPrimitiveTypeWithMultiplicity, SType}
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1

object DataflowAnalysis {


  case class EntityDefinition(attributes: Seq[Attribute])


  case class EntityRef(name: String)
  case class AttributeRef(name: String)
  case class AttributeIndex(entityRef: EntityRef, attributeRef: AttributeRef){
    override def toString: String = s"${entityRef.name}.${attributeRef.name}"
  }

  sealed trait Attribute{
    def index: AttributeIndex
  }
  case class Property(index: AttributeIndex, tpe: Type) extends Attribute
  case class DerivedValue(index: AttributeIndex) extends Attribute
  case class Relation(index: AttributeIndex, entityRef: EntityRef, multiplicity: Multiplicity) extends Attribute


  sealed trait Multiplicity

  trait PossibleZero {this: Multiplicity => }
  trait NonZero extends PossibleZero {this: Multiplicity =>}

  case object One extends Multiplicity with NonZero
  case object ZeroOrOne extends Multiplicity with PossibleZero
  case object ZeroOrMore extends Multiplicity with PossibleZero
  case object OneOrMore extends Multiplicity with NonZero

  sealed trait Type
  case object StringType extends Type
  case object BooleanType extends Type
  case object IntType extends Type
  case object FloatType extends Type

  case class DataflowGraph(nodes: Seq[Attribute], edges: Seq[Edge])

  case class Edge(from: AttributeIndex, to: AttributeIndex)
  case class DependencyCalculation(attribute: AttributeIndex, e: SExp)

  case class CollectDefinitions(nodes: Seq[Attribute], calculations: Seq[DependencyCalculation])

  def mapMultiplicity(m: SMultiplicity): Multiplicity = m match {
    case One0(_) => One
    case ZeroOrOne0(_) => ZeroOrOne
    case ZeroOrMore0(_) => ZeroOrMore
    case OneOrMore0(_) => OneOrMore
  }

  def mapType(m: SPrimitiveTypeWithMultiplicity): Type = m match {
    case PrimitiveTypeWithMultiplicity2(tpe, _, _) => mapType(tpe)
    case PrimitiveTypeWithDefaultMultiplicity1(tpe, _) => mapType(tpe)
  }

  def mapType(m: SType): Type = m match {
    case String0(_) => StringType
    case Boolean0(_) => BooleanType
    case Int0(_) => IntType
    case Float0(_) => FloatType
  }

  def dataflowAnalysis(start1: Start1): DataflowGraph = {
    val CollectDefinitions(nodes, calculations) = collectDefinitions(start1)
    val edges = calculations.flatMap(dependencyCalculation(nodes))
    DataflowGraph(nodes, edges)
  }

  def collectDefinitions(start1: Start1): CollectDefinitions = {
    val initial = CollectDefinitions(Seq.empty, Seq.empty)
    start1.model1.value.foldLeft(initial){
      case (defs, Entity2(id1, member2, origin)) => member2.value.foldLeft(defs)(collectMember(EntityRef(id1.string)))
      case (defs, Relation6(EntityRef1(e1, _), AttributeRef1(a1, _), m1, m2, EntityRef1(e2, _), AttributeRef1(a2, _), _)) => defs.copy(
        nodes =
          Relation(AttributeIndex(EntityRef(e1.string), AttributeRef(a1.string)), EntityRef(e2.string), mapMultiplicity(m1)) +:
          Relation(AttributeIndex(EntityRef(e2.string), AttributeRef(a2.string)), EntityRef(e1.string), mapMultiplicity(m2)) +:
          defs.nodes
      )
    }
  }

  def collectMember(entity: EntityRef)(collectDefinitions: CollectDefinitions, member: SMember): CollectDefinitions = {
    member match {
      case Attribute2(name, tpe, _) => collectDefinitions.copy(
        nodes = Property(AttributeIndex(entity, AttributeRef(name.string)), mapType(tpe)) +: collectDefinitions.nodes
      )
      case DerivedAttribute3(id1, multiplicity3, exp, _) => collectDefinitions.copy(
        nodes = DerivedValue(AttributeIndex(entity, AttributeRef(id1.string))) +: collectDefinitions.nodes,
        calculations = DependencyCalculation(AttributeIndex(entity, AttributeRef(id1.string)), exp) +: collectDefinitions.calculations
      )
    }
  }

  def dependencyCalculation(nodes: Seq[Attribute])(d: DependencyCalculation): Set[Edge] = {
    def walk(index: AttributeIndex, e: SExp): (AttributeIndex, Set[Edge]) = e match {
      case _: SLiteral => (index, Set.empty)
      case MemberAccess2(exp1, id2, origin) =>
        val (next, edges) = walk(index, exp1)
        nodes.find(_.index == next).map{
          case Relation(index, entity, multiplicity) => (AttributeIndex(entity, AttributeRef(id2.string)), edges ++ Set(Edge(d.attribute, AttributeIndex(entity, AttributeRef(id2.string)))))
          case Property(to, _) => (index, edges ++ Set(Edge(index, to)))
          case DerivedValue(to) => (index, edges ++ Set(Edge(index, to)))
        }.get

      case BinExp(_, lhs, rhs) => (index, walk(index, lhs)._2 ++ walk(index, rhs)._2)
      case UnExp(_, e) => walk(index, e)
      case Ref1(id1, origin) => (index.copy(attributeRef = AttributeRef(id1.string)), Set(Edge(index, index.copy(attributeRef = AttributeRef(id1.string)))))
      case This0(origin) => (index, Set.empty)
      case Apply2(Ref1(_, _), exps, _) => (index, exps.value.flatMap(e => walk(index, e)._2).to[Set])
      case Apply2(e1, exps, _) => (index, walk(index, e1)._2 ++ exps.value.flatMap(e => walk(index, e)._2))
    }
    walk(d.attribute, d.e)._2
  }


  def stronglyConnected(graph: DataflowGraph): Seq[Seq[AttributeIndex]] = {
    import org.metaborg.entitylang.util.MapExtensions._

    class Vertice(var index: Int, var lowlink: Int, var onStack: Boolean, val node: AttributeIndex)
    type AdjacencyList = Map[Vertice, Seq[Vertice]]

    val UNDEFINED = -1
    var index = 0
    var sccs = Seq.empty[Seq[AttributeIndex]]
    val s = scala.collection.mutable.Stack.apply[Vertice]()
    val vertices = graph.nodes.map(n => new Vertice(-1, 0, false, n.index))
    val verticeMap = vertices.map(v => v.node -> v).toMap
    val adjacencyList = graph.edges.foldLeft[AdjacencyList](graph.nodes.map(n => verticeMap(n.index) -> Seq.empty).toMap){case (adj, Edge(from, to)) => adj.addBinding(verticeMap(from), verticeMap(to))}

    val edges = for{
      (from, adj) <- adjacencyList.toList
      to <- adj
    } yield (verticeMap(from.node), verticeMap(to.node))


    for(v <- vertices if v.index == UNDEFINED){
      run(v)
    }

    def run(v: Vertice): Unit ={
      v.index = index
      v.lowlink = index
      index = index + 1
      s.push(v)
      v.onStack = true

      for(w <- adjacencyList(v)){
        if(w.index == UNDEFINED){
          run(w)
          v.lowlink = Math.min(v.lowlink, w.lowlink)
        } else if(w.onStack){
          v.lowlink = Math.min(v.lowlink, w.index)
        }
      }

      if(v.lowlink == v.index){
        var scc = Seq.empty[AttributeIndex]
        var w: Vertice = null
        do {
          w = s.pop()
          w.onStack = false
          scc = w.node +: scc
        } while(v != w)
        sccs = scc +: sccs
      }
    }
    sccs.reverse
  }
}
