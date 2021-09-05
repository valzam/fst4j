import scala.io.Source
import scala.collection.mutable

object Fixture {
  val start = "<<start>>"
  val end = "<<end>>"
}

case class Node(val content: String, children: mutable.HashSet[Node] = mutable.HashSet[Node]()) {
  override def equals(o: Any) = o match {
    case that: Node => that.content.equalsIgnoreCase(this.content)
    case _          => false
  }
  override def hashCode = content.toUpperCase.hashCode
}

case class FST() {
  val index = mutable.Map[String, Node]()
  val start = Node(Fixture.start)
  val end = Node(Fixture.end)
  index.addOne(Fixture.start -> start)
  index.addOne(Fixture.end -> end)

  def build(s: Seq[String]) = {
    s.foreach(x => {
      val s = x.split(" ").toList.prepended(Fixture.start)
      processSentence(s, index)
    })
  }

  def contains(s: String, currentNode: Node = start): Boolean = contains(s.split(" ").toList, start)

  private def contains(s: Seq[String], currentNode: Node): Boolean =
    s match {
      case hd :: Nil =>
        currentNode.children.find(_.content == hd) match {
          case Some(value) => value.children.contains(end)
          case None        => false
        }
      case hd :: tail =>
        currentNode.children.find(_.content == hd) match {
          case Some(value) => contains(tail, index.get(hd).get)
          case None        => false
        }
      case Nil =>
        false
    }

  def hasPrefix(s: String, currentNode: Node = start): Boolean = hasPrefix(s.split(" ").toList, start)

  private def hasPrefix(s: Seq[String], currentNode: Node): Boolean =
    s match {
      case hd :: Nil =>
        currentNode.children.find(_.content.startsWith(hd)).isDefined
      case hd :: tail =>
        currentNode.children.find(_.content.startsWith(hd)) match {
          case Some(value) => hasPrefix(tail, index.get(hd).get)
          case None        => false
        }
      case Nil =>
        true
    }

  private def processSentence(s: Seq[String], heap: mutable.Map[String, Node]): Unit = {
    s match {
      case hd :: Nil =>
        processLastToken(hd, heap)
      case hd :: tail :: rest =>
        processToken(hd, tail, heap)
        processSentence(tail :: rest, heap)
      case Nil => ()
    }
  }

  private def processLastToken(t: String, heap: mutable.Map[String, Node]) = {
    heap.get(t) match {
      case Some(existingNode) => existingNode.children.add(end)
      case None               => heap.addOne(t -> Node(t, mutable.HashSet(end)))
    }
  }

  private def processToken(t: String, tail: String, heap: mutable.Map[String, Node]) = {
    val child = heap.get(tail) match {
      case Some(value) => value
      case None => {
        val newNode = Node(tail)
        heap.addOne(tail, newNode)
        newNode
      }
    }
    heap.get(t) match {
      case Some(existingNode) => existingNode.children.add(child)
      case None               => heap.addOne(t -> Node(t, mutable.HashSet(child)))
    }
  }
}

val a = Source.fromFile("output.txt").getLines().toList.sorted
// val a = List("hello my world", "hello my schmoi")
val f = FST()
f.build(a)
f.index
f.contains("siri play harry potter on the tv")
f.hasPrefix("siri play harry potter")
