package org.riedelcastro.lmp

import gurobi._
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author sriedel
 */
trait CuttingPlaneProblem extends VariationalProblem {
  this: FactorGraphEnv with Polytope with SeparationOracle =>

  var iterations = 0

  abstract override def solve() = {
    var mu: MeanVector = null
    var cut: Option[Constraint] = None

    iterations = 0

    do {
      mu = super.solve()
      cut = separate(mu)
      for (c <- cut) addConstraint(c)
      iterations += 1
    } while (cut.isDefined)
    mu
  }
}



trait SeparationOracle {
  this: VariationalProblem =>

  def separate(meanVector: MeanVector): Option[Constraint]

}











