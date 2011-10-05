package org.riedelcastro.lmp.graph

import java.io.PrintStream
import io.Source

/**
 * @author sriedel
 */
trait NautyAutomorphismFinder extends AutomorphismFinder {
  this: Graph =>

  private val dreadnaut = "/Users/riedelcastro/opt/nauty24r2/dreadnaut"

  private def exec(cmd: String) = Runtime.getRuntime exec cmd

  private def toIndices(string: String): Seq[Int] = {
    def fromRange(range:String):Seq[Int] = {
      if (range.contains(":")) {
        val Array(from, to) = range.split(":")
        Range(from.trim().toInt, to.trim().toInt + 1)
      } else Seq(range.toInt)
    }
    for (range <- string.split(" "); index <- fromRange(range)) yield index
  }


  def orbits(partition: Seq[Seq[V]]) = {
    val process = exec(dreadnaut)
    val out = new PrintStream(process.getOutputStream)

    val n = vertices.size
    out.println("n = " + n)

    //edges
    out.println("g")
    for (edge <- edges) {
      out.println("%d:%d".format(edge.source.index, edge.target.index))
    }
    out.println(".")

    //partition
    out.println("f=[%s]".format(partition.map(_.map(_.index).mkString(",")).mkString("|")))

    //run orbit finder and print results
    out.println("x")
    out.println("o")
    out.close()


    //scan output
    val source = Source.fromInputStream(process.getInputStream)
    val lines = source.getLines().toSeq
    val tc = lines.indexWhere(_.startsWith("tctotal"))
    for (line <- lines) {
      println(line)
    }
    val indexStrings = lines.drop(tc + 1).map(_.trim).mkString(" ").split(";").map(_.trim)
    println(indexStrings.mkString("\n"))
    val indices = indexStrings.map(toIndices(_))
    val result = indices.map(_.map(index => vertices(index)))

    result
  }
}

trait AutomorphismFinder {
  this: Graph =>

  def orbits(partition: Seq[Seq[V]]): Seq[Seq[V]]

}

