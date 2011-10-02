package org.riedelcastro.lmp

import sun.net.www.content.text.Generic

/**
 * @author sriedel
 */
object SemiTransitiveModelTest {

  import SemiTransitiveModel._

  def main(args: Array[String]) {
    val env = new SimpleFactorGraphEnv[Pred, Boolean] with SemiTransitiveModel
    val scores = Map(
      Seq(false, false) -> -1.0,
      Seq(false, true) -> 0.0,
      Seq(true, false) -> 0.0,
      Seq(true, true) -> -1.0)
    val fg = env.createSemiTransitiveFG(3, scores)
    val local = new ProxyEnv(env) with GurobiLocalMAP
    local.addFG(fg)
    val localMu = local.solve()
    println(localMu)

    val cycles = new ProxyEnv(env) with GurobiLocalMAP with CuttingPlaneProblem with CycleSeparationOracle
    cycles.addFG(fg)
    val cycleMu = cycles.solve()
    println(cycleMu)



  }

}

object SemiTransitiveModel {
  case class Pred(x: Int, y: Int)

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

}

