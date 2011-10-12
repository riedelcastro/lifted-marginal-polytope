package org.riedelcastro.lmp

import graph.{NautyAutomorphismFinder, SimpleGraph}
import collection.mutable.ArrayBuffer

/**
 * @author sriedel
 */
trait NautyLifter {
  this: FactorGraphEnv =>

  val dreadnaut = "/Users/riedelcastro/opt/nauty24r2/dreadnaut"

  case class Orbits(nodeOrbits: Seq[Seq[N]], factorOrbits: Seq[Seq[F]])

  def exec(cmd: String) = Runtime.getRuntime exec cmd

  def toIndices(string: String): Seq[Int] = {
    if (string.contains(":")) {
      val Array(from, to) = string.split(":")
      Range(from.trim().toInt, to.trim().toInt + 1)
    } else string.trim().split(" ").map(_.trim.toInt)
  }


  def findOrbits(fg:FG, nodeOrbits:Seq[Seq[N]]):Orbits = {
    val factorOrbits = fg.factors.groupBy(_.potential).map(_._2).toSeq
    findOrbits(fg,nodeOrbits,factorOrbits)
  }

  def findOrbits(fg:FG, nodeOrbits:Seq[Seq[N]], factorOrbits:Seq[Seq[F]]):Orbits = {
    sealed trait Label
    case class Node(n:N) extends Label
    case class Factor(f:F) extends Label
    case class Arg(f:F,index:Int) extends Label

    val graph = new SimpleGraph[Label] with NautyAutomorphismFinder {}

    for (node <- fg.nodes){
      graph.addVertex(Node(node))
    }

    val args = new ArrayBuffer[Arg]

    for (factor <- fg.factors){
      val factorV = graph.addVertex(Factor(factor))
      for ((node,index) <- factor.nodes.zipWithIndex) {
        val arg = Arg(factor,if (factor.potential.symmetric) 0 else index)
        val argV = graph.addVertex(arg)
        val nodeV = graph.vertex(Node(node))
        graph.addEdge(factorV,argV)
        graph.addEdge(argV,nodeV)
        args += arg
      }
    }

    //val argOrbitVertices = graph.verticesOfLabelType[Arg].groupBy(_.index).map(_._2)
    val argOrbitVertices = args.groupBy(_.index).map(_._2.map(arg => graph.vertex(arg)))
    val nodeOrbitVertices = nodeOrbits.map(_.map(n => graph.vertex(Node(n))))
    val factorOrbitVertices = factorOrbits.map(_.map(f => graph.vertex(Factor(f))))

    val preOrbits =nodeOrbitVertices ++ factorOrbitVertices ++ argOrbitVertices

    println(argOrbitVertices.mkString("\n"))
    println(nodeOrbitVertices.mkString("\n"))
    println(factorOrbitVertices.mkString("\n"))

    val result = graph.orbits(preOrbits)

    val nodeResult = result.filter(_.head.label.isInstanceOf[Node]).map(_.map(_.label.asInstanceOf[Node].n))
    val factorResult = result.filter(_.head.label.isInstanceOf[Factor]).map(_.map(_.label.asInstanceOf[Factor].f))

    Orbits(nodeResult,factorResult)

  }



}

object TestNautyLifter {
  def main(args: Array[String]) {
    val env = new SimpleFactorGraphEnv[Int, Boolean]
    val domain = Seq(true, false)
    import env._
    val nodes = for (i <- 0 until 4) yield createNode(i, domain)
    val pot = createPotential({case _ => 0.0})
    val factors = for (i <- nodes; j <- nodes; if (i.id != j.id)) yield createFactor(Seq(i, j), pot)
    val fg = env.createFG(nodes, factors)
    val lifter = new ProxyEnv(env) with NautyLifter
    val orbits = lifter.findOrbits(fg, Seq.empty, fg.factors.groupBy(_.potential).map(_._2).toSeq)
    println(orbits.nodeOrbits.mkString("\n"))
    println(orbits.factorOrbits.mkString("\n"))

  }
}









