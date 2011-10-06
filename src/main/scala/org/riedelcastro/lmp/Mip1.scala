package org.riedelcastro.lmp

import gurobi._


/**
 * @author sriedel
 */
object Mip1 {
  def main(args: Array[String]) {
    try {
      val env = new GRBEnv("mip1.log")
      val model = new GRBModel(env)

      // Create variables

      val x = model.addVar(0.0, 1.0, -1.0, GRB.BINARY, "x")
      val y = model.addVar(0.0, 1.0, -1.0, GRB.BINARY, "y")
      val z = model.addVar(0.0, 1.0, -2.0, GRB.BINARY, "z")

      model.update()

      // Add constraint: x + 2 y + 3 z <= 4

      var expr = new GRBLinExpr()
      expr.addTerm(1.0, x)
      expr.addTerm(2.0, y)
      expr.addTerm(3, z)
      model.addConstr(expr, GRB.LESS_EQUAL, 4.0, "c0")

      // Add constraint: x + y >= 1

      expr = new GRBLinExpr()
      expr.addTerm(1.0, x)
      expr.addTerm(1.0, y)
      model.addConstr(expr, GRB.GREATER_EQUAL, 1.0, "c1")

      // Optimize model

      model.optimize()

      println(x.get(GRB.StringAttr.VarName)
        + " " + x.get(GRB.DoubleAttr.X))
      println(y.get(GRB.StringAttr.VarName)
        + " " + y.get(GRB.DoubleAttr.X))
      println(z.get(GRB.StringAttr.VarName)
        + " " + z.get(GRB.DoubleAttr.X))
      println("Obj: " + model.get(GRB.DoubleAttr.ObjVal))


    } catch {
      case e: GRBException =>
        println("Error code: " + e.getErrorCode + ". " + e.getMessage)
    }
  }
}

object SemitransitiveModelOld {

  def main(args: Array[String]) {
    case class Node(x: Int, y: Int) {
      override def toString = "pred(%d,%d)".format(x, y)
    }
    case class Edge(arg1: Node, arg2: Node) {
      override def toString = "%s-%s".format(arg1, arg2)
    }

    val domain = 0 until 3
    val values = 0 until 2
    val nodes = (for (x <- domain; y <- domain; if (x != y)) yield (x, y) -> Node(x, y)).toMap
    val edges = for (x <- domain;
                     y <- domain; if (y != x);
                     z <- domain; if (z != y && z != x)) yield Edge(nodes(x -> y), nodes(y -> z))
    val allNodes = nodes.values

    val scores = Map(
      (0, 0) -> -1.0,
      (0, 1) -> 1.0,
      (1, 0) -> 1.0,
      (1, 1) -> -1.0).mapValues(s => -s)

    try {
      val env = new GRBEnv("mip1.log")
      val model = new GRBModel(env)

      //marginal polytope variables
      val varsNode = (for (node <- allNodes; v <- values) yield {
        val modelVar = model.addVar(0.0, 1.0, 0.0, GRB.CONTINUOUS, "mu_pred(%d,%d)_%d".format(node.x, node.y, v))
        (node, v) -> modelVar
      }).toMap
      val varsEdge = (for (edge <- edges; v1 <- values; v2 <- values) yield {
        val modelVar = model.addVar(0.0, 1.0, scores(v1 -> v2), GRB.CONTINUOUS,
          "mu_pred(%d,%d),pred(%d,%d)_%d,%d".format(edge.arg1.x, edge.arg1.y, edge.arg2.x, edge.arg2.y, v1, v2))
        (edge, v1, v2) -> modelVar
      }).toMap


      model.update()

      //constraint sum to 1
      for (node <- allNodes) {
        val vars = values.map(v => varsNode(node -> v))
        val expr = new GRBLinExpr()
        for (v <- vars) {
          expr.addTerm(1.0, v)
        }
        model.addConstr(expr, GRB.EQUAL, 1.0, "uniq_" + node)
      }

      //local consistency
      for (edge <- edges) {
        def addConstraint(varNode: GRBVar, varFactors: IndexedSeq[GRBVar], v: Int): GRBConstr = {
          val expr = new GRBLinExpr()
          expr.addTerm(-1.0, varNode)
          for (varFactor <- varFactors) expr.addTerm(1.0, varFactor)
          model.addConstr(expr, GRB.EQUAL, 0.0, "local_%s_%d".format(edge, v))
        }
        for (v <- values) {
          val varNode = varsNode(edge.arg1 -> v)
          val varFactors = values.map(v2 => varsEdge((edge, v, v2)))
          addConstraint(varNode, varFactors, v)
        }
        for (v <- values) {
          val varNode = varsNode(edge.arg2 -> v)
          val varFactors = values.map(v2 => varsEdge((edge, v2, v)))
          addConstraint(varNode, varFactors, v)
        }
      }
      //let's add a single cycle
      def addCycle(node1: Node, node2: Node, node3: Node) {
        val edge1 = Edge(node1, node2)
        val edge2 = Edge(node2, node3)
        val edge3 = Edge(node3, node1)
        val G = Seq(edge1, edge2, edge3)

        def addCycle(F1: Seq[Edge], G: Seq[Edge]) {
          val R1 = G.filter(edge => !F1.contains(edge))
          val cycle1 = new GRBLinExpr()
          for (edge <- R1) {
            cycle1.addTerm(1.0, varsEdge((edge, 0, 1)))
            cycle1.addTerm(1.0, varsEdge((edge, 1, 0)))
          }
          for (edge <- F1) {
            cycle1.addTerm(1.0, varsEdge((edge, 0, 0)))
            cycle1.addTerm(1.0, varsEdge((edge, 1, 1)))
          }
          model.addConstr(cycle1, GRB.GREATER_EQUAL, 1.0, "cycle1")
        }

        addCycle(G, G)
        addCycle(Seq(edge1), G)
        addCycle(Seq(edge2), G)
        addCycle(Seq(edge3), G)
      }
      addCycle(Node(0, 1), Node(1, 2), Node(2, 0))
      addCycle(Node(1, 0), Node(0, 2), Node(2, 1))




      // Optimize model
      model.optimize()

      for (edge <- edges)
        for (v1 <- values; v2 <- values) {
          //      for (nodeVar <- varsNode.values) {
          val edgeVar = varsEdge((edge, v1, v2))
          println("%30s: %f %f".format(edgeVar.get(GRB.StringAttr.VarName),
            edgeVar.get(GRB.DoubleAttr.X), edgeVar.get(GRB.DoubleAttr.Obj)))
        }


      for (node <- allNodes) {
        //      for (nodeVar <- varsNode.values) {
        val nodeVar = varsNode(node -> 0)
        println("%30s: %f".format(nodeVar.get(GRB.StringAttr.VarName), nodeVar.get(GRB.DoubleAttr.X)))
      }


    } catch {
      case e: GRBException =>
        e.printStackTrace()
        println("Error code: " + e.getErrorCode + ". " + e.getMessage)
    }


  }

}


object LiftedSemiTransitiveModel {
  def main(args: Array[String]) {
    try {
      val env = new GRBEnv("mip1.log")
      val model = new GRBModel(env)
      val scores = Map(
        (0, 0) -> -1.0,
        (0, 1) -> 1.0,
        (1, 0) -> 1.0,
        (1, 1) -> -1.0).mapValues(s => -s)


      val values = 0 until 2
      val liftedNodeVars = {
        for (v <- values) yield {
          val nodeVar = model.addVar(0.0, 1.0, 0.0, GRB.CONTINUOUS, "mu_pred_%d".format(v))
          v -> nodeVar
        }
      }.toMap

      val liftedFactorVars = {
        for (v1 <- values; v2 <- values) yield {
          val nodeVar = model.addVar(0.0, 1.0, scores(v1,v2), GRB.CONTINUOUS, "mu_pred_%d,%d".format(v1, v2))
          (v1, v2) -> nodeVar
        }
      }.toMap

      model.update()
      //uniqueness constraint
      val uniq = new GRBLinExpr()
      uniq.addTerm(1.0, liftedNodeVars(0))
      uniq.addTerm(1.0, liftedNodeVars(1))
      model.addConstr(uniq, GRB.EQUAL, 1.0, "uniq")

      //consistency
      for (v <- values){
        val expr1 = new GRBLinExpr()
        expr1.addTerm(1.0, liftedNodeVars(v))
        expr1.addTerm(-1.0, liftedFactorVars(v,0))
        expr1.addTerm(-1.0, liftedFactorVars(v,1))
        model.addConstr(expr1, GRB.EQUAL, 0.0, "c1_" + v)
        val expr2 = new GRBLinExpr()
        expr2.addTerm(1.0, liftedNodeVars(v))
        expr2.addTerm(-1.0, liftedFactorVars(0,v))
        expr2.addTerm(-1.0, liftedFactorVars(1,v))
        model.addConstr(expr2, GRB.EQUAL, 0.0, "c2_" + v)
      }

      //cycle1 F=G
      val cycle1 = new GRBLinExpr()
      cycle1.addTerm(1.0, liftedFactorVars(0,0))
      cycle1.addTerm(1.0, liftedFactorVars(1,1))
      model.addConstr(cycle1, GRB.GREATER_EQUAL, 1.0/3.0, "cycle1")

      val cycle2 = new GRBLinExpr()
      cycle2.addTerm(2.0, liftedFactorVars(0,1))
      cycle2.addTerm(2.0, liftedFactorVars(1,0))
      cycle2.addTerm(1.0, liftedFactorVars(0,0))
      cycle2.addTerm(1.0, liftedFactorVars(1,1))
      model.addConstr(cycle2, GRB.GREATER_EQUAL, 1.0, "cycle2")


      model.optimize()

      for (v1 <- values; v2 <- values){
        val variable = liftedFactorVars(v1,v2)
        println("%30s: %f %f".format(variable.get(GRB.StringAttr.VarName),
          variable.get(GRB.DoubleAttr.X), variable.get(GRB.DoubleAttr.Obj)))

      }
      val variable = liftedNodeVars(0)
      println("%30s: %f".format(variable.get(GRB.StringAttr.VarName), variable.get(GRB.DoubleAttr.X)))


    } catch {
      case e: GRBException =>
        e.printStackTrace()
        println("Error code: " + e.getErrorCode + ". " + e.getMessage)
    }
  }
}