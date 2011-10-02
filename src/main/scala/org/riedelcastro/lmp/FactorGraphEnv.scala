package org.riedelcastro.lmp

/**
 * @author sriedel
 */

trait FactorGraphEnv {
  self =>

  import FactorGraphEnv._

  type Id
  type V
  type N <: Node[Id, V]
  type P <: Potential[V]
  type F <: Factor[Id, V, N, P]
  type FG <: FactorGraph[Id, V, N, P, F]

  def createNode(id: Id, domain: Seq[V]): N
  def createFactor(nodes: Seq[N], potential: P): F
  def createPotential(func: PartialFunction[Seq[V], Double]): P
  def createFG(nodes: Seq[N], factors: Seq[F]): FG


}

object FactorGraphEnv {

  trait Node[Id, V] {
    def id: Id
    def domain: Seq[V]
  }
  trait Potential[V] {
    def func: PartialFunction[Seq[V], Double]
  }
  trait Factor[Id, V, N <: Node[Id, V], P <: Potential[V]] {
    def nodes: Seq[N]
    def potential: P
    def assignments = Util.assignments(nodes.map(_.domain))
  }
  trait FactorGraph[Id, V, N <: Node[Id, V], P <: Potential[V], F <: Factor[Id, V, N, P]] {
    def nodes: Seq[N]
    def factors: Seq[F]
  }

  case class SimpleNode[Id, V](id: Id, domain: Seq[V])
    extends Node[Id, V] {
    override def toString = id.toString
  }
  case class SimpleFactor[Id, V, N <: Node[Id, V], P <: Potential[V]](nodes: Seq[N], potential: P)
    extends Factor[Id, V, N, P] {
    override def toString = nodes.mkString("Factor(", ",", ")")
  }

  case class SimpleFactorWithContent[Id, V, N <: Node[Id, V], P <: Potential[V],C](nodes: Seq[N], potential: P, content:C)
    extends Factor[Id, V, N, P] {
    override def toString = nodes.mkString("Factor(", ",", ")")
  }


  case class Edge[Id, V, N <: Node[Id, V], P <: Potential[V]](arg1: N, arg2: N, potential: P) extends Factor[Id, V, N, P] {
    def nodes = Seq(arg1, arg2)
  }

  case class Unary[Id, V, N <: Node[Id, V], P <: Potential[V]](arg: N, potential: P) extends Factor[Id, V, N, P] {
    def nodes = Seq(arg)
  }


  case class SimplePotential[V](func: PartialFunction[Seq[V], Double]) extends Potential[V]
  case class SimpleFactorGraph[Id, V, N <: Node[Id, V], P <: Potential[V], F <: Factor[Id, V, N, P]](nodes: Seq[N],
                                                                                                     factors: Seq[F])
    extends FactorGraph[Id, V, N, P, F]


}


class ProxyEnv[Underlying <: FactorGraphEnv](val underlying: Underlying) extends Proxy with FactorGraphEnv {
  def self = underlying
  type V = underlying.V
  type F = underlying.F
  type FG = underlying.FG
  type Id = underlying.Id
  type N = underlying.N
  type P = underlying.P
  def createFactor(nodes: Seq[N], potential: P) = underlying.createFactor(nodes, potential)
  def createFG(nodes: Seq[N], factors: Seq[F]) = underlying.createFG(nodes, factors)
  def createNode(id: Id, domain: Seq[V]) = underlying.createNode(id, domain)
  def createPotential(func: PartialFunction[Seq[V], Double]) = underlying.createPotential(func)
}


class SimpleFactorGraphEnv[ID, Value] extends FactorGraphEnv {

  import FactorGraphEnv._

  type Id = ID
  type V = Value
  def createFactor(nodes: Seq[N], potential: P) = SimpleFactor(nodes, potential)
  def createNode(id: Id, domain: Seq[V]) = SimpleNode(id, domain)
  def createPotential(func: PartialFunction[Seq[V], Double]) = SimplePotential(func)
  def createFG(nodes: Seq[N], factors: Seq[F]) = SimpleFactorGraph(nodes, factors)
  type F = SimpleFactor[Id, V, N, P]
  type N = SimpleNode[Id, V]
  type P = SimplePotential[V]
  type FG = SimpleFactorGraph[Id, V, N, P, F]

}

class SimpleFactorGraphEnvWithContent[ID, Value, FC] extends FactorGraphEnv {

  import FactorGraphEnv._

  type Id = ID
  type V = Value
  type FactorContent = FC
  def createFactor(nodes: Seq[N], potential: P) =
    SimpleFactorWithContent[Id,V,N,P,FactorContent](nodes, potential,null.asInstanceOf[FactorContent])
  def createFactor(nodes: Seq[N], potential: P, content:FactorContent) =
    SimpleFactorWithContent[Id,V,N,P,FactorContent](nodes, potential,content)
  def createNode(id: Id, domain: Seq[V]) = SimpleNode(id, domain)
  def createPotential(func: PartialFunction[Seq[V], Double]) = SimplePotential(func)
  def createFG(nodes: Seq[N], factors: Seq[F]) = SimpleFactorGraph(nodes, factors)
  type F = SimpleFactorWithContent[Id, V, N, P, FactorContent]
  type N = SimpleNode[Id, V]
  type P = SimplePotential[V]
  type FG = SimpleFactorGraph[Id, V, N, P, F]


}


trait BinaryEdgeGraphEnv extends FactorGraphEnv {

  import FactorGraphEnv._

  type F <: Edge[Id, V, N, P]

}

class SimpleBinaryEdgeGraphEnv[ID, Value] extends BinaryEdgeGraphEnv {

  import FactorGraphEnv._

  type Id = ID
  type V = Value
  def createFactor(nodes: Seq[N], potential: P) = {
    require(nodes.size == 2)
    Edge(nodes(0), nodes(1), potential)
  }

  def createNode(id: Id, domain: Seq[V]) = SimpleNode(id, domain)
  def createPotential(func: PartialFunction[Seq[V], Double]) = SimplePotential(func)
  def createFG(nodes: Seq[N], factors: Seq[F]) = SimpleFactorGraph(nodes, factors)
  type F = Edge[Id, V, N, P]
  type N = SimpleNode[Id, V]
  type P = SimplePotential[V]
  type FG = SimpleFactorGraph[Id, V, N, P, F]
}










































