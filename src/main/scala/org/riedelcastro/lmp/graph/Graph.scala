package org.riedelcastro.lmp.graph

import collection.mutable.{HashMap, ArrayBuffer}
import org.riedelcastro.lmp.graph.Graph.Vertex

/**
 * @author sriedel
 */
trait Graph extends OrderedHyperGraph {

  type V <: Graph.Vertex
  type E <: Graph.Edge[V]

}

object Graph {
  trait Vertex extends OrderedHyperGraph.Vertex

  trait Edge[V <: Vertex] extends OrderedHyperGraph.Edge[V] {
    def source: V
    def target: V
    def nodes = Seq(source,target)
  }
}


trait OrderedHyperGraph {
  type V <: OrderedHyperGraph.Vertex
  type E <: OrderedHyperGraph.Edge[V]

  def vertices: Seq[V]
  def edges: Seq[E]


}

object OrderedHyperGraph {
  trait Vertex {
    def index: Int
  }
  trait Edge[V <: Vertex] {
    def nodes: Seq[V]
  }
}

trait MutableGraph extends Graph {

  def addVertex(): V
  def addEdge(source: V, target: V): E

}

object LabelledGraph {

  import Graph._

  trait LabelledVertex[L] extends Vertex {
    def label: L
  }
}

trait LabelledGraph extends Graph {

  type L
  type V <: LabelledGraph.LabelledVertex[L]

  def getVertex(label: L): Option[V]
  def vertex(label: L) = getVertex(label).get

}

trait MutableLabelledGraph extends LabelledGraph {
  def getOrElseAddVertex(label: L): V = {
    getVertex(label).getOrElse(addVertex(label))
  }
  def addVertex(label: L): V
}


trait WeightedGraph extends Graph {

  type E <: WeightedGraph.WeightedEdge[V]

}

trait MutableWeightedGraph extends WeightedGraph {
  def addEdge(source: V, target: V, weight: Double): E

}

object WeightedGraph {

  import Graph._

  trait WeightedEdge[V <: Vertex] extends Edge[V] {
    def weight: Double
  }
}

object SimpleGraph {
  case class Vertex[L](label: L, index: Int) extends LabelledGraph.LabelledVertex[L]
  case class Edge[L, V <: Vertex[L]](source: V, target: V, weight: Double) extends WeightedGraph.WeightedEdge[V]
}

trait SimpleGraph[Label] extends MutableLabelledGraph with MutableWeightedGraph {


  import SimpleGraph._
  import LabelledGraph.LabelledVertex


  type L = Label
  type V = SimpleGraph.Vertex[L]
  type E = SimpleGraph.Edge[L, V]

  val vertices = new ArrayBuffer[V]
  val edges = new ArrayBuffer[E]
  val label2vertex = new HashMap[L, V]

  def addVertex(label: L) = {
    val vertex = Vertex(label, vertices.size)
    vertices += vertex
    label2vertex(label) = vertex
    vertex
  }


  def getVertex(label: L) = label2vertex.get(label)

  def addEdge(source: V, target: V, weight: Double = 0.0) = {
    val edge = Edge[L, V](source, target, weight)
    edges += edge
    edge
  }
}

trait GraphProxy extends Graph {
  type Underlying <: Graph
  val underlying:Underlying
  type E = underlying.E
  type V = underlying.V
  def edges = underlying.edges
  def vertices = underlying.vertices
}

trait MutableWeightedGraphProxy extends GraphProxy with WeightedGraph {
  type Underlying <: MutableWeightedGraph

  def addEdge(source: V, target: V, weight: Double) = underlying.addEdge(source,target,weight)

}

trait FactorGraph extends OrderedHyperGraph {

  type Value
  type Potential <: FactorGraph.Potential[Value]
  type V <: FactorGraph.Node[Value]
  type E <: FactorGraph.Factor[Value,V,Potential]

  def factors = edges
  def nodes = vertices
}

object FactorGraph {

  trait Potential[V] {
    def score(args:Seq[V]):Double
  }

  trait Node[V] extends OrderedHyperGraph.Vertex {
    def domain:Seq[V]
  }

  trait Factor[V, N<:Node[V], P<:Potential[V]] extends OrderedHyperGraph.Edge[N] {
    def potential:P
  }

}



