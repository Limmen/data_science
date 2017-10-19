package limmen.kth.se
import akka.actor.ActorRef

trait Vertex {

  def vertexId() : Int
  def getSuperstep() : Int
  def getValue(): Double
  def mutableValue(newValue: Double): Unit
  def getOutEdges(): List[Int]
  def sendMessageTo(v : Int, worker : ActorRef, message : Message) : Unit
  def compute(messages: List[Message], superstep: Int, workers: Map[Int, ActorRef]) : Boolean

}
