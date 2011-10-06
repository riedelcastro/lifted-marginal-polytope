package org.riedelcastro.lmp

import org.riedelcastro.lmp.FactorGraphEnv._
import org.jgrapht.graph.{SimpleWeightedGraph, DefaultWeightedEdge}
import collection.mutable.{ArrayBuffer, HashMap}
import collection.JavaConversions._
import org.jgrapht.alg.DijkstraShortestPath


object LiftedProblem {

  case class NodeOrbit[N](nodes: Seq[N]) {
    def example = nodes.head
  }
  case class FactorOrbit[F](factors: Seq[F]) {
    def example = factors.head
  }

  case class NodeSpec[N](node: NodeOrbit[N], copy1: Boolean)
  case class FactorSpec[F](factor: FactorOrbit[F], copy1: Boolean, copy2: Boolean) extends DefaultWeightedEdge {
    def sameCopy = copy1 == copy2
    //    lazy val varPair = {
    //      if (sameCopy)
    //        FactorValueVar(factor, Seq(false, true)) -> FactorValueVar(factor, Seq(true, false))
    //      else
    //        FactorValueVar(factor, Seq(false, false)) -> FactorValueVar(factor, Seq(true, true))
    //    }
    //    lazy val varSeq = Seq(varPair._1,varPair._2)

  }
}
/**
 * @author sriedel
 */
trait LiftedProblem extends VariationalProblem with NautyLifter {
  self: FactorGraphEnv {type V = Boolean} =>

  import LiftedProblem._


  def addToSearchSpace(factor: F) {}
  def addToSearchSpace(node: N) {}
  def addToObjective(factor: F) {}
  def addToObjective(node: N) {}

  val liftedEnv = new SimpleFactorGraphEnvWithContent[NodeOrbit[N], V, FactorOrbit[F]]

  val domain = Seq(true, false)

  var findCycles = true

  class PerOrbitCycleGraph(val orbit: NodeOrbit[N]) {
    val spGraph = new SimpleWeightedGraph[NodeSpec[N], FactorSpec[F]](classOf[FactorSpec[F]])
    val node2orbit = new HashMap[N, NodeOrbit[N]]
    val example = orbit.example
    val exampleOrbit = NodeOrbit(Seq(example))
    //val edge2varPair = new HashMap[FactorSpec[F],Tuple2[]]

  }

  def solve() = {

    //find orginal orbits
    val fg = createFG(nodes, factors)
    val orbits: Orbits = findOrbits(fg, Seq.empty)


    val nodeOrbits = orbits.nodeOrbits.map(NodeOrbit(_))
    val factorOrbits: Seq[liftedEnv.FactorContent] = orbits.factorOrbits.map(FactorOrbit(_))



    //need mapping from nodes to orbit
    val node2Orbit = nodeOrbits.flatMap(orbit => orbit.nodes.map(node => node -> orbit)).toMap
    val factor2Orbit = factorOrbits.flatMap(orbit => orbit.factors.map(f => f -> orbit)).toMap


    //create orbit nodes
    val orbit2Node = {
      for (orbit <- nodeOrbits) yield {
        val node = liftedEnv.createNode(orbit, domain)
        orbit -> node
      }
    }.toMap

    //orbit to lifted factor mapping
    val orbit2Factor = {
      for (orbit <- factorOrbits) yield {
        val neigbors = orbit.example.nodes.map(prop => {
          val nodeOrbit = node2Orbit(prop)
          val orbitNode = orbit2Node(nodeOrbit)
          orbitNode
        })
        val pot = liftedEnv.createPotential({case x => orbit.example.potential.func(x) * orbit.factors.size})
        orbit -> liftedEnv.createFactor(neigbors, pot, orbit)
        //val factor = liftedEnv.createF

      }
    }.toMap

    //for each orbit, choose one example and create a partition for it
    val cycleGraphs = new ArrayBuffer[PerOrbitCycleGraph]

    if (findCycles) {
      for (nodeOrbit <- nodeOrbits) {
        val cycleGraph = new PerOrbitCycleGraph(nodeOrbit)
        cycleGraphs += cycleGraph
        import cycleGraph._
        val rest = nodeOrbit.nodes.filter(_ != example)
        val otherParts = nodeOrbits.filter(_ != nodeOrbit).map(_.nodes)
        val refinedPartition = Seq(Seq(example), rest) ++ otherParts
        val refinedOrbits = findOrbits(fg, refinedPartition, orbits.factorOrbits)

        //for each new orbit, create two vertices
        for (part <- refinedOrbits.nodeOrbits) {
          val orbit = NodeOrbit(part)
          spGraph.addVertex(NodeSpec(orbit, false))
          spGraph.addVertex(NodeSpec(orbit, true))
          for (node <- part) node2orbit(node) = orbit
        }

        //for each (binary) factor orbit create set of edges
        for (part <- refinedOrbits.factorOrbits) {
          val factor = part.head
          if (factor.nodes.size == 2) {
            //figure out the node orbits to connect to
            val arg1 = node2orbit(factor.nodes(0))
            val arg2 = node2orbit(factor.nodes(1))

            val factorOrbit = FactorOrbit(part)

            val spec11 = FactorSpec(factorOrbit, true, true)
            val spec00 = FactorSpec(factorOrbit, false, false)
            val spec01 = FactorSpec(factorOrbit, false, true)
            val spec10 = FactorSpec(factorOrbit, true, false)

            if (arg1 != arg2) {
              spGraph.addEdge(NodeSpec(arg1, true), NodeSpec(arg2, true), spec11)
              spGraph.addEdge(NodeSpec(arg1, false), NodeSpec(arg2, false), spec00)
            }

            spGraph.addEdge(NodeSpec(arg1, false), NodeSpec(arg2, true), spec01)
            spGraph.addEdge(NodeSpec(arg1, true), NodeSpec(arg2, false), spec10)

          }
        }

      }
    }



    val liftedNodes = orbit2Node.values.toSeq
    val liftedFactors = orbit2Factor.values.toSeq

    println(liftedNodes.mkString("\n"))
    println(liftedFactors.mkString("\n"))

    val liftedFG = liftedEnv.createFG(liftedNodes, liftedFactors)

    val liftedMapProblem = new ProxyEnv(liftedEnv) with GurobiLocalMAP

    liftedMapProblem.addFG(liftedFG)

    var liftedSolution = liftedMapProblem.solve()

    println(liftedSolution)

    if (findCycles) {
      var constraint: Option[liftedMapProblem.Constraint] = None
      do {
        val paths = for (cg <- cycleGraphs) yield {
          //go over edges set weights
          for (spEdge <- cg.spGraph.edgeSet()) {
            //need to find a factor in common lifted graph
            val example = spEdge.factor.example
            val lifted = factor2Orbit(example)
            val liftedFactor = orbit2Factor(lifted)
            val weight = if (spEdge.sameCopy) {
              val mu01 = liftedSolution.factors(liftedMapProblem.FactorValueVar(liftedFactor, Seq(false, true)))
              val mu10 = liftedSolution.factors(liftedMapProblem.FactorValueVar(liftedFactor, Seq(true, false)))
              mu01 + mu10
            } else {
              val mu00 = liftedSolution.factors(liftedMapProblem.FactorValueVar(liftedFactor, Seq(false, false)))
              val mu11 = liftedSolution.factors(liftedMapProblem.FactorValueVar(liftedFactor, Seq(true, true)))
              mu00 + mu11
            }
            cg.spGraph.setEdgeWeight(spEdge, weight)
          }
          val from = NodeSpec(cg.exampleOrbit, true)
          val to = NodeSpec(cg.exampleOrbit, false)

          val sp = new DijkstraShortestPath(cg.spGraph, from, to)
          sp.getPath
        }

        val shortest = paths.minBy(_.getWeight)
        def varPair(spEdge: FactorSpec[F]) = {
          val example = spEdge.factor.example
          val lifted = factor2Orbit(example)
          val liftedFactor = orbit2Factor(lifted)
          if (spEdge.sameCopy) {
            liftedMapProblem.FactorValueVar(liftedFactor, Seq(false, true)) ->
              liftedMapProblem.FactorValueVar(liftedFactor, Seq(true, false))
          } else {
            liftedMapProblem.FactorValueVar(liftedFactor, Seq(false, false)) ->
              liftedMapProblem.FactorValueVar(liftedFactor, Seq(true, true))
          }
        }
        def varSeq(spEdge: FactorSpec[F]) = {
          val pair = varPair(spEdge)
          Seq(pair._1, pair._2)
        }

        constraint = if (shortest.getWeight < 1.0) {
          val terms = for (spEdge <- shortest.getEdgeList; v <- varSeq(spEdge)) yield liftedMapProblem.Term(1.0, v)
          val constraint = liftedMapProblem.Constraint(terms, liftedMapProblem.GEQ, 1.0)
          Some(constraint)
        } else {
          None
        }
        for (c <- constraint) {
          liftedMapProblem.addConstraint(c)
          liftedSolution = liftedMapProblem.solve()
        }


      } while (constraint.isDefined)

    }



    new MeanVector {
      val factors = {
        for (f <- self.factors;
             orbit = factor2Orbit(f);
             liftedFactor = orbit2Factor(orbit);
             x <- f.assignments) yield {
          val factorValueVar = liftedMapProblem.FactorValueVar(liftedFactor, x)
          FactorValueVar(f, x) -> liftedSolution.factors(factorValueVar)
        }
      }.toMap
      val variables = {
        for (n <- self.nodes;
             orbit = node2Orbit(n);
             liftedNode = orbit2Node(orbit);
             x <- n.domain) yield {
          val nodeValueVar = liftedMapProblem.NodeValueVar(liftedNode, x)
          NodeValueVar(n, x) -> liftedSolution.variables(nodeValueVar)
        }
      }.toMap
    }
  }
}

/*
1) Iterate over all orbits in O \in Aut(G)

If we use Aut(G), then we assume that our pairwise potentials are symmetric, that is \phi(x,y) = \phi(y,x). If the potentials are non symmetric then we need to turn G into a directed graph G_d first, then compute Aut(G_d)


2.1) Select one instance o \in O and create Aut_o(G), create lifted graph L_o from Aut_o(G)

This lifted graph doesn't depend on the choice of o, so it is safe to call it L_O
In this graph, there can be loop (edge connect a node i to itself), and multi-edge (more than two edges joining i and j).

Note: in this step, there is no need to assign direction to G as in step 1.

2.2) Create duplicate graph L_o' from L_o.

Multi-edges can be removed from the duplicated graph L'_O by replacing all the edges joining i and j by the minimum edge. This is fine since we are only using L'_O to find shortest path, and all weights are non negative.


2.3) Find shortest path P_O in L_o', using lifted marginals

As I've noted in my previous email, in this step we only find the short path from o_1 to o_2


*/