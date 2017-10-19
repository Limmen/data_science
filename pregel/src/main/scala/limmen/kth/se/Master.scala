package limmen.kth.se
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

/*
 * Pregel Master
 */
class Master(graph: List[Vertex], workers: Map[Int, ActorRef], system: ActorSystem) extends Actor with ActorLogging {
  var superstep = 1
  var superstepMsgs: Set[(Int, Int)] = Set()
  var result: Set[(Int, List[(Int, Any)])] = Set()

  /*
   * Assign partitions to workers and start first superstep
   */
  override def preStart(): Unit = {
    val workerSize = workers.size
    log.info(s"Initializing $workerSize workers")
    workers.map((worker) => {
      val vs = graph.filter((v: Vertex) => v.vertexId() % workerSize == worker._1)
      worker._2 ! new VertexPartition(vs, self)
      worker._2 ! new Workers(workers)
    })
    superStepInit()
  }

  /*
   * ReceiveLoop, collect superstep results and decide to continue or to aggregate results.
   */
  def receive = {
    case SuperStepDone(id, active) =>
      superstepMsgs = superstepMsgs + ((id, active))
      if (superstepMsgs.size == workers.size)
        superStepDone()
    case Result(id, res) => {
      val r = (id, res)
      result = result + (r)
      if (result.size == workers.size)
        printResult()
    }
  }

  /*
   * Initialize new superstep
   */
  def superStepInit(): Unit = {
    log.info(s"Starting Superstep $superstep")
    workers.map((worker) => {
      worker._2 ! SuperStepStart(superstep)
    })
    superstep = superstep + 1
  }

  /*
   * Complete Superstep and decide if continue or collect results
   */
  def superStepDone(): Unit = {
    val active = superstepMsgs.toList.map((t) => t._2).sum
    log.info(s"Superstep done, active vertices: $active")
    if (active > 0)
      superStepInit()
    else
      workers.map((worker) => worker._2 ! Done)
    superstepMsgs = Set()
  }

  /*
   * Print results
   */
  def printResult(): Unit = {
    val vertexResult = result.flatMap((t) => t._2)
    log.info("Result: ")
    vertexResult.foreach((v) => log.info(s"Vertex: ${v._1}, Value: ${v._2}"))
    system.terminate()
  }
}

object Master {
  def props(graph: List[Vertex], workers: Map[Int, ActorRef], system: ActorSystem): Props = {
    Props(new Master(graph, workers, system))
  }
}

case object Done

case class VertexPartition(vertices: List[Vertex], master: ActorRef)

case class SuperStepStart(num: Int)

case class Workers(workers: Map[Int, ActorRef])
