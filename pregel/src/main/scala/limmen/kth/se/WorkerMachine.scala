package limmen.kth.se
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

/*
 * Pregel Worker
 */
class WorkerMachine(id: Int) extends Actor with ActorLogging {
  var activeVertices: Set[Vertex] = Set()
  var vertices: Set[Vertex] = Set()
  var workers: Map[Int, ActorRef] = Map()
  var vertexMsgs: Map[Int, List[Message]] = Map()
  var master: ActorRef = _

  /*
   * Message-receive loop, receive vertex partitions, receive superstep start,
   * receive computation done and receive vertex-messages from other workers
   */
  def receive = {
    case VertexPartition(vs, m: ActorRef) => {
      log.info(s"Received Partition: ${vs.size}")
      master = m
      vertices = Set()
      activeVertices = Set()
      vertices = vertices ++ vs
      activeVertices = activeVertices ++ vs
      vertices.map((v) => vertexMsgs += (v.vertexId() -> List()))
    }
    case Workers(ws) => {
      workers = ws
    }
    case SuperStepStart(n) => {
      log.info(s"Starting Superstep: $n")
      superstep(n)
    }
    case VertexMessage(vertex: Int, m: Message) => {
      val oldList = vertexMsgs(vertex)
      val newList = m :: oldList
      vertexMsgs += (vertex -> newList)
    }
    case Done => {
      val result = vertices.map((v) => (v.vertexId, v.getValue)).toList
      master ! new Result(id, result)
    }
  }

  /*
   * Execute superstep
   */
  def superstep(n: Int): Unit = {
    vertices.map(v => {
      if (vertexMsgs(v.vertexId()).size > 0)
        activeVertices = activeVertices + v
    })
    val activeCount = activeVertices.toList.map((v) => {
      v.compute(vertexMsgs(v.vertexId()), n, workers)
    }).filter((b) => b).size
    vertexMsgs = Map()
    vertices.map((v) => vertexMsgs += (v.vertexId() -> List()))
    master ! new SuperStepDone(id, activeCount)
  }
}

case class VertexMessage(vertex: Int, m: Message)

case class SuperStepDone(id: Int, active: Int)

case class Result(id: Int, vertexState: List[(Int, Any)])

object WorkerMachine {
  def props(id: Int): Props = {
    Props(new WorkerMachine(id))
  }
}
