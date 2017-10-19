package limmen.kth.se
import akka.actor.ActorRef

/*
 * Vertex-algorithm for computing PageRank with Pregel
 */
class PageRankVertex(vertexId: Int, initValue: Double, outEdges: List[Int], numVertices: Int) extends Vertex {

  private var vertexValue = initValue
  private var superstep = 0

  def vertexId(): Int = {
    vertexId
  }

  def getValue(): Double = {
    vertexValue
  }

  def mutableValue(newValue: Double): Unit = {
    vertexValue = newValue
  }

  def getSuperstep(): Int = {
    superstep
  }

  def getOutEdges(): List[Int] = {
    outEdges
  }

  def sendMessageTo(v: Int, worker: ActorRef, message: Message): Unit = {
    worker ! new VertexMessage(v, message)
  }

  /*
   * Compute new pagerank given incoming messages and send out votes to neighbors, max 30 iterations
   */
  def compute(msgs: List[Message], st: Int, workers: Map[Int, ActorRef]): Boolean = {
    superstep = st
    if (superstep >= 1) {
      val sum = msgs.map((m) => {
        m match {
          case pm: PageRankMessage => pm.rank
        }
      }).sum
      val newVal = (0.15 / numVertices) + 0.85 * sum
      mutableValue(newVal)
    }
    if (superstep < 30) {
      val n = outEdges.size
      val workerSize = workers.size
      outEdges.map((v) => {
        val workerId = v % workerSize
        val worker = workers(workerId)
        sendMessageTo(v, worker, new PageRankMessage(vertexValue / n))
      })
      return true
    } else
      return false
  }

}
