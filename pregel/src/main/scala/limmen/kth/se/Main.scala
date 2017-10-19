package limmen.kth.se

import akka.actor.ActorSystem
import com.github.tototoshi.csv.CSVReader
import java.io.File
import org.rogach.scallop.ScallopConf

/*
 * Command-line arguments
 */
class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val vertex = opt[Int](required = true)
  val workers = opt[Int](required = true)
  verify()
}

/*
 * Program entrypoint
 */
object Main {
  def main(args: Array[String]) = {
    val system: ActorSystem = ActorSystem("pregel")
    val conf = new Conf(args)
    val vertex = conf.vertex()
    val workerCount = conf.workers()
    val graphRaw = CSVReader.open(new File("data/graph.csv")).all()
    val graph = preProcess(graphRaw, vertex)
    val workers = (0 until workerCount).map((i: Int) => i -> system.actorOf(WorkerMachine.props(i), i.toString)).toMap
    val master = system.actorOf(Master.props(graph, workers, system), "master")
  }

  /*
   * Preprocess graph from .csv fil
   */
  def preProcess(rawGraph: List[List[String]], vertex: Int): List[Vertex] = {
    vertex match {
      case 1 => pageRankGraph(rawGraph)
    }
  }

  /*
   * Parse rawGaph into graph of PageRank vertices
   */
  def pageRankGraph(rawGraph: List[List[String]]): List[Vertex] = {
    val initValue = 1 / rawGraph.size
    rawGraph.map((v: List[String]) => {
      val v2 = v.map((str) => str.toInt)
      val vertexId = v2(0)
      val neighBors = v2.drop(1)
      new PageRankVertex(vertexId, initValue.toDouble, neighBors, rawGraph.size)
    })
  }
}
