package org.riedelcastro.lmp

import collection.mutable.ArrayBuffer

/**
 * @author sriedel
 */
trait VariationalProblem extends SearchSpace with Objective {
  this: FactorGraphEnv =>

  val nodes = new ArrayBuffer[N]
  val factors = new ArrayBuffer[F]

  def addFG(fg: FG) {
    for (node <- fg.nodes) addNode(node)
    for (factor <- fg.factors) addFactor(factor)
  }

  def addNode(node: N) {
    addToSearchSpace(node)
    addToObjective(node)
    nodes += node
  }

  def addFactor(factor: F) {
    addToSearchSpace(factor)
    addToObjective(factor)
    factors += factor
  }

  sealed trait Var
  case class NodeValueVar(node: N, value: V) extends Var {
    override def toString = node.toString + "=" + value.toString
  }
  case class FactorValueVar(factor: F, value: Seq[V]) extends Var {
    override def toString = factor.toString + "=" + value.mkString(",")
  }
  case class Term(coeff: Double, variable: Var)

  sealed trait Comp
  object LEQ extends Comp
  object GEQ extends Comp
  object EQ extends Comp

  case class Constraint(vars: Seq[Term], comp: Comp, rhs: Double)

  trait MeanVector {

    def nodeAvg(nodeValues:Seq[(N,V)]) = {
      nodeValues.map(pair => variables(NodeValueVar(pair._1,pair._2))).sum / nodeValues.size
    }

    def variables: Map[NodeValueVar, Double]
    def factors: Map[FactorValueVar, Double]
    override def toString = {
      val vs = variables.map({case (v,s) => "%-20s: %f".format(v,s)}).mkString("\n")
      val fs = factors.map({case (f,s) => "%-20s: %f".format(f,s)}).mkString("\n")
      vs + "\n" + fs
    }
     def nodesString = {
      val vs = variables.map({case (v,s) => "%-20s: %f".format(v,s)}).mkString("\n")
      vs
    }

  }

  def solve(): MeanVector

}

trait SearchSpace {
  this: FactorGraphEnv =>

  def addToSearchSpace(node: N)
  def addToSearchSpace(factor: F)

}

trait Objective {
  this: FactorGraphEnv =>

  def addToObjective(node: N)
  def addToObjective(factor: F)

}

trait LocalPolytope extends SearchSpace {
  this: FactorGraphEnv =>


  def addToSearchSpace(node: N) {
    ensureSumTo1(node)
  }

  def addToSearchSpace(factor: F) {
    for ((node, index) <- factor.nodes.zipWithIndex; v <- node.domain) ensureConsistency(factor, index, v)
  }

  def ensureSumTo1(node: N)
  def ensureConsistency(factor: F, nodeIndex: Int, value: V)

}

trait MAPObjective extends Objective {
  this: FactorGraphEnv with VariationalProblem =>

  def addToObjective(node: N) {}
  def addToObjective(factor: F) {addFactorToLinearObjective(factor)}
  def addFactorToLinearObjective(factor: F)

}

trait Polytope {
  this: FactorGraphEnv with VariationalProblem =>


  def addConstraint(constraint: Constraint)

}

trait LinearObjective {

  this:FactorGraphEnv with VariationalProblem =>

  def addTermToObjective(term:Term)

}

trait GenericMAPObjective extends MAPObjective {

  this: FactorGraphEnv with LinearObjective with VariationalProblem =>

  def addFactorToLinearObjective(factor: F) {
    val domains = factor.nodes.map(_.domain)
    val xs = Util.assignments(domains)
    for (x <- xs) {
      val score = factor.potential.func(x)
      val term = Term(score,FactorValueVar(factor,x))
      addTermToObjective(term)
    }
  }

}

trait GenericLocalPolytope extends LocalPolytope {
  this: FactorGraphEnv with VariationalProblem with Polytope =>

  def ensureConsistency(factor: F, nodeIndex: Int, value: V) {
    val node = factor.nodes(nodeIndex)
    val domains = factor.nodes.zipWithIndex.map({case (n,i) => if (i == nodeIndex) Seq(value) else n.domain})
    val factorTerms = Util.assignments(domains).map(x => Term(1.0, FactorValueVar(factor, x)))
    val nodeTerm = Term(-1.0, NodeValueVar(node, value))
    val terms = factorTerms :+ nodeTerm
    val cut = Constraint(terms, EQ, 0.0)
    addConstraint(cut)

  }
  def ensureSumTo1(node: N) {
    val terms = node.domain.map(v => Term(1.0, NodeValueVar(node, v)))
    val cut = Constraint(terms, EQ, 1.0)
    addConstraint(cut)
  }
}


