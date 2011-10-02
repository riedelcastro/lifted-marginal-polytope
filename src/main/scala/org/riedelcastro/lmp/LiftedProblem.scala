package org.riedelcastro.lmp

import org.riedelcastro.lmp.FactorGraphEnv._


object LiftedProblem {
  case class NodeOrbit[N](nodes:Seq[N]) {
    def example = nodes.head
  }
  case class FactorOrbit[F](factors:Seq[F]) {
    def example = factors.head
  }

}
/**
 * @author sriedel
 */
trait LiftedProblem extends VariationalProblem with NautyLifter {
  self:FactorGraphEnv { type V = Boolean }=>

  import LiftedProblem._


  def addToSearchSpace(factor: F) {}
  def addToSearchSpace(node: N) {}
  def addToObjective(factor: F) {}
  def addToObjective(node: N) {}

  val liftedEnv = new SimpleFactorGraphEnvWithContent[NodeOrbit[N],V,FactorOrbit[F]]

  val domain = Seq(true,false)

  def solve() = {

    //find orginal orbits
    val orbits:Orbits = findOrbits(createFG(nodes,factors),Seq.empty, Seq.empty)

    val nodeOrbits = orbits.nodeOrbits.map(NodeOrbit(_))
    val factorOrbits:Seq[liftedEnv.FactorContent] = orbits.factorOrbits.map(FactorOrbit(_))

    //need mapping from nodes to orbit
    val node2Orbit = nodeOrbits.flatMap(orbit => orbit.nodes.map(node => node -> orbit)).toMap
    val factor2Orbit = factorOrbits.flatMap(orbit => orbit.factors.map(f => f -> orbit)).toMap


    //create orbit nodes
    val orbit2Node = {for (orbit <- nodeOrbits) yield {
      val node = liftedEnv.createNode(orbit,domain)
      orbit -> node
    }}.toMap

    //orbit to lifted factor mapping
    val orbit2Factor = {for (orbit <- factorOrbits) yield {
      val neigbors = orbit.example.nodes.map(prop => {
        val nodeOrbit = node2Orbit(prop)
        val orbitNode = orbit2Node(nodeOrbit)
        orbitNode
      })
      val pot = liftedEnv.createPotential({case x => orbit.example.potential.func(x) * orbit.factors.size})
      orbit -> liftedEnv.createFactor(neigbors,pot,orbit)
      //val factor = liftedEnv.createF
    }}.toMap

    val liftedNodes = orbit2Node.values.toSeq
    val liftedFactors = orbit2Factor.values.toSeq

    val liftedFG = liftedEnv.createFG(liftedNodes,liftedFactors)

    val liftedMapProblem = new ProxyEnv(liftedEnv) with GurobiLocalMAP

    liftedMapProblem.addFG(liftedFG)

    val liftedSolution = liftedMapProblem.solve()

    new MeanVector {
      val factors = {
        for (f <- self.factors;
             orbit = factor2Orbit(f);
             liftedFactor = orbit2Factor(orbit);
             x <- f.assignments) yield {
          val factorValueVar = liftedMapProblem.FactorValueVar(liftedFactor, x)
          FactorValueVar(f,x) -> liftedSolution.factors(factorValueVar)
        }
      }.toMap
      val variables = {
        for (n <- self.nodes;
             orbit = node2Orbit(n);
             liftedNode = orbit2Node(orbit);
             x <- n.domain) yield {
          val nodeValueVar = liftedMapProblem.NodeValueVar(liftedNode, x)
          NodeValueVar(n,x) -> liftedSolution.variables(nodeValueVar)
        }
      }.toMap
    }





    null
  }
}