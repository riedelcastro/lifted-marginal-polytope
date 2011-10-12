package org.riedelcastro.lmp

import gurobi._
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author sriedel
 */
trait GurobiProblem extends VariationalProblem with Polytope with LinearObjective {
  this: FactorGraphEnv =>

  protected val env = new GRBEnv("mip1.log")
  protected val model = new GRBModel(env)

  protected val vars = new HashMap[Var, GRBVar]
  protected val factorVars = new ArrayBuffer[FactorValueVar]
  protected val nodeVars = new ArrayBuffer[NodeValueVar]

  protected var updated = false

  var objective = 0.0

  def getOrCreateVar(v: Var): GRBVar = {
    vars.getOrElseUpdate(v, {
      updated = false
      v match {
        case n: NodeValueVar => nodeVars += n
        case f: FactorValueVar => factorVars += f
      }
      model.addVar(0.0, 1.0, 0.0, GRB.CONTINUOUS, v.toString)
    })
  }

  def update() {
    if (!updated) {
      model.update()
      updated = true
    }
  }

  def toGurobiComp(comp: Comp) = {
    comp match {
      case LEQ => GRB.LESS_EQUAL
      case GEQ => GRB.GREATER_EQUAL
      case EQ => GRB.EQUAL
    }
  }

  def addConstraint(cut: Constraint) {
    val expr = new GRBLinExpr()
    for (term <- cut.vars) {
      val v = getOrCreateVar(term.variable)
      expr.addTerm(term.coeff, v)
    }
    update()
    model.addConstr(expr, toGurobiComp(cut.comp), cut.rhs, cut.toString)
  }


  def addTermToObjective(term: Term) {
    val v = getOrCreateVar(term.variable)
    update()
    v.set(GRB.DoubleAttr.Obj, -term.coeff)
    println("Coefficient for %s set to %f".format(term.variable,-term.coeff))
  }

  def solve() = {
    update()
    //model.getEnv.set(GRB.IntParam.Presolve, 0)
    model.optimize()

//    model.write("test.mps")
//    model.write("test.lp")


    objective = model.get(GRB.DoubleAttr.ObjVal)

    val factorMeans = factorVars.map(v => {
      val gurobiVar = getOrCreateVar(v)
      val result = gurobiVar.get(GRB.DoubleAttr.X)
      v -> result
    })

    val nodeMeans = nodeVars.map(v => {
      val gurobiVar = getOrCreateVar(v)
      val result = gurobiVar.get(GRB.DoubleAttr.X)
      v -> result
    })

    new MeanVector {
      def factors = factorMeans.toMap
      def variables = nodeMeans.toMap
    }
  }
}

trait GurobiLocalMAP extends GurobiProblem with GenericLocalPolytope with GenericMAPObjective  {
  this: FactorGraphEnv =>

}






object Util {
  def assignments[V](domains: Seq[Seq[V]], left: Seq[Seq[V]] = Seq(Seq.empty[V])): Seq[Seq[V]] = {
    if (domains.isEmpty) left
    else {
      val domain = domains.head
      val generated = for (old <- left; v <- domain) yield old ++ Seq(v)
      assignments(domains.drop(1), generated)
    }
  }
}