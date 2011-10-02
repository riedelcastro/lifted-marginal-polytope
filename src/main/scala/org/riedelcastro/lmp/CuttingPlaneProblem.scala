package org.riedelcastro.lmp

import gurobi._
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author sriedel
 */
trait CuttingPlaneProblem extends VariationalProblem {
  this: FactorGraphEnv with Polytope with SeparationOracle =>

  abstract override def solve() = {
    var mu: MeanVector = null
    var cut: Option[Constraint] = None

    do {
      mu = super.solve()
      cut = separate(mu)
      for (c <- cut) addConstraint(c)
    } while (cut.isDefined)
    mu
  }
}



trait SeparationOracle {
  this: VariationalProblem =>

  def separate(meanVector: MeanVector): Option[Constraint]

}











