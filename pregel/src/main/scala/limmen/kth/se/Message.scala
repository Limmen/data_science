package limmen.kth.se

trait Message {
}

case class PageRankMessage(rank: Double) extends Message
