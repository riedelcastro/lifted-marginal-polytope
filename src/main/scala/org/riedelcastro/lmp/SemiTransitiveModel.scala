package org.riedelcastro.lmp

import util.Random
import org.riedelcastro.lmp.LiftedProblem.NodeOrbit

/**
 * @author sriedel
 */
object SemiTransitiveModelTest {

  import SemiTransitiveModel._


  def main(args: Array[String]) {
    val timer = new Timer()
    val env = new SimpleFactorGraphEnv[Pred, Boolean] with SemiTransitiveModel
    val scores = Map(
      Seq(false, false) -> -1.0,
      Seq(false, true) -> 0.0,
      Seq(true, false) -> 0.0,
      Seq(true, true) -> -1.0)
    val fg = env.createSemiTransitiveFGWithLocalFactors(4, 1, 0.1, scores)
    val local = new ProxyEnv(env) with GurobiLocalMAP
    local.addFG(fg)
    val localMu = timer.time("local",local.solve())
    println(localMu)

    val lifted = new ProxyEnv(env) with LiftedProblem
    lifted.addFG(fg)
    val liftedMu = timer.time("lifted",lifted.solve())
    println(liftedMu)

    val cycles = new ProxyEnv(env) with GurobiLocalMAP with CuttingPlaneProblem with CycleSeparationOracle
    cycles.addFG(fg)
    val cycleMu = timer.time("cycles",cycles.solve())
    println(cycleMu)

    for (nodeOrbit <- lifted.nodeOrbits) {
      val localAvg = localMu.nodeAvg(nodeOrbit.nodes.map(_ -> true))
      val cycleAvg = cycleMu.nodeAvg(nodeOrbit.nodes.map(_ -> true))
      val liftedAvg = liftedMu.nodeAvg(nodeOrbit.nodes.map(_ -> true))

      println(nodeOrbit.nodes.map(_.id).mkString(" "))
      println("%5f %5f %5f".format(localAvg,cycleAvg,liftedAvg))

    }
    println("Local:  %f".format(local.objective))
    println("Cycles: %f".format(cycles.objective))
    println("Lifted: %f".format(lifted.objective))

    println(timer)
    println("Cycle iterations:  " + cycles.iterations)
    println("Lifted iterations: " + lifted.iterations)



  }

}

object SemiTransitiveModel {
  case class Pred(x: Int, y: Int)

  val random = new Random(0)


}

trait SemiTransitiveModel extends FactorGraphEnv {

  import SemiTransitiveModel._

  override type Id = Pred
  override type V = Boolean

  val domain = Vector(true, false)

  def createSemiTransitiveFG(size: Int, scores: PartialFunction[Seq[Boolean], Double]) = {
    val I = 0 until size
    val pot = createPotential(scores)
    val nodes = (for (x <- I; y <- I; if (x != y)) yield (x, y) -> createNode(Pred(x, y), domain)).toMap
    val factors = for (x <- I; y <- I; if (x != y); z <- I; if (z != y && z != x)) yield {
      createFactor(Vector(nodes(x, y), nodes(y, z)), pot)
    }
    createFG(nodes.values.toSeq, factors)
  }


  def createSemiTransitiveFGWithLocalFactors(size: Int,
                                             localFactorCount: Int = 0,
                                             localScore: Double,
                                             scores: PartialFunction[Seq[Boolean], Double]) = {
    val I = 0 until size
    val pot = createPotential(scores, true)
    val posLocalPot = createPotential({case Seq(true) => localScore; case _ => 0.0})
    val negLocalPot = createPotential({case Seq(true) => -localScore; case _ => 0.0})
    val nodes = (for (x <- I; y <- I; if (x != y)) yield (x, y) -> createNode(Pred(x, y), domain)).toMap
    val (pos,neg) = nodes.values.splitAt(localFactorCount)
    val local =       pos.map(node => createFactor(Vector(node), posLocalPot)) ++
      neg.map(node => createFactor(Vector(node), negLocalPot))
    val binary = for (x <- I; y <- I; if (x != y); z <- I; if (z != y && z != x)) yield {
      createFactor(Vector(nodes(x, y), nodes(y, z)), pot)
    }
    createFG(nodes.values.toSeq, binary ++ local)
  }

  def createSemiTransitiveFGWithRandomLocalFactors(size: Int,
                                             prob: Double = 1.0,
                                             localScore: Double,
                                             scores: PartialFunction[Seq[Boolean], Double]) = {
    val I = 0 until size
    val pot = createPotential(scores)
    val localPot = createPotential({case Seq(true) => localScore; case _ => 0.0})
    val nodes = (for (x <- I; y <- I; if (x != y)) yield (x, y) -> createNode(Pred(x, y), domain)).toMap
    val local = for (node <- nodes.values; if (SemiTransitiveModel.random.nextDouble() < prob)) yield {
      createFactor(Vector(node), localPot)
    }
    val binary = for (x <- I; y <- I; if (x != y); z <- I; if (z != y && z != x)) yield {
      createFactor(Vector(nodes(x, y), nodes(y, z)), pot)
    }
    createFG(nodes.values.toSeq, binary ++ local)
  }


}

