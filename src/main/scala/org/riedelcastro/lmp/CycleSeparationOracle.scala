package org.riedelcastro.lmp

import gurobi._
import org.jgrapht._
import alg.DijkstraShortestPath
import collection.mutable.{HashSet, ArrayBuffer, HashMap}
import graph._
import org.jgrapht.graph.{DefaultWeightedEdge, SimpleWeightedGraph}

/**
 * @author sriedel
 */
trait CycleSeparationOracle extends SeparationOracle with VariationalProblem {

  this: FactorGraphEnv {type V = Boolean} =>

  var eps = 0.00001

  case class NodeSpec(node: N, copy1: Boolean)
  case class FactorSpec(factor: F, copy1: Boolean, copy2: Boolean) extends DefaultWeightedEdge {
    def sameCopy = copy1 == copy2
    lazy val varPair = {
      if (sameCopy)
        FactorValueVar(factor, Seq(false, true)) -> FactorValueVar(factor, Seq(true, false))
      else
        FactorValueVar(factor, Seq(false, false)) -> FactorValueVar(factor, Seq(true, true))
    }
    lazy val varSeq = Seq(varPair._1,varPair._2)


  }

  val spGraph = new SimpleWeightedGraph[NodeSpec, FactorSpec](classOf[FactorSpec])
  val nodesInSpGraph = new HashSet[N]

  override def addFactor(factor: F) {
    super.addFactor(factor)
    if (factor.nodes.size == 2) {
      val arg1 = factor.nodes(0)
      val arg2 = factor.nodes(1)
      nodesInSpGraph += arg1
      nodesInSpGraph += arg2
      spGraph.addVertex(NodeSpec(arg1, true))
      spGraph.addVertex(NodeSpec(arg1, false))
      spGraph.addVertex(NodeSpec(arg2, true))
      spGraph.addVertex(NodeSpec(arg2, false))
      spGraph.addEdge(NodeSpec(arg1, true), NodeSpec(arg2, true), FactorSpec(factor, true, true))
      spGraph.addEdge(NodeSpec(arg1, false), NodeSpec(arg2, false), FactorSpec(factor, false, false))
      spGraph.addEdge(NodeSpec(arg1, false), NodeSpec(arg2, true), FactorSpec(factor, false, true))
      spGraph.addEdge(NodeSpec(arg1, true), NodeSpec(arg2, false), FactorSpec(factor, true, false))
    }
    //build shortest path finding graph, but only for binary edges
  }

  def separate(meanVector: MeanVector) = {
    import scala.collection.JavaConversions._
    //put marginals onto weighted graph
    for (spEdge <- spGraph.edgeSet()) {
      val (var1,var2) = spEdge.varPair
      val weight = meanVector.factors(var1) + meanVector.factors(var2)
      spGraph.setEdgeWeight(spEdge, weight)
    }
    //run shortest path on path finding graph for each node in graph
    val paths: Iterable[GraphPath[NodeSpec, FactorSpec]] = for (node <- nodesInSpGraph) yield {
      val from = NodeSpec(node, true)
      val to = NodeSpec(node, false)
      val sp = new DijkstraShortestPath(spGraph, from, to)
      sp.getPath
    }

    //find minimum path of all
    val shortest = paths.minBy(_.getWeight)
    if (shortest.getWeight < 1.0 - eps) {
      val terms = for (spEdge <- shortest.getEdgeList; v <- spEdge.varSeq) yield Term(1.0,v)
      val constraint = Constraint(terms,GEQ, 1.0)
      Some(constraint)
    } else
      None
  }
}








