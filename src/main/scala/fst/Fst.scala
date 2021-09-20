package fst

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
      processSentence(s)
    })
  }

  def contains(s: String, currentNode: Node = start): Boolean = contains(s.split(" ").toList, start)

  private def contains(s: Seq[String], currentNode: Node): Boolean =
    s match {
      case hd :: Nil =>
        currentNode.children.find(_.content == hd) match {
          case Some(foundNode) => foundNode.children.contains(end)
          case None            => false
        }
      case hd :: tail =>
        currentNode.children.find(_.content == hd) match {
          case Some(foundNode) => contains(tail, foundNode)
          case None            => false
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
          case Some(foundNode) => hasPrefix(tail, foundNode)
          case None            => false
        }
      case Nil =>
        true
    }

  private def processSentence(s: Seq[String]): Unit = {
    s match {
      case hd :: Nil =>
        processLastToken(hd)
      case hd :: tail :: rest =>
        processToken(hd, tail)
        processSentence(tail :: rest)
      case Nil => ()
    }
  }

  private def processLastToken(t: String) = {
    index.get(t) match {
      case Some(existingNode) => existingNode.children.add(end)
      case None               => index.addOne(t -> Node(t, mutable.HashSet(end)))
    }
  }

  private def processToken(t: String, tail: String) = {
    val child = this.index.get(tail) match {
      case Some(value) => value
      case None => {
        val newNode = Node(tail)
        this.index.addOne(tail, newNode)
        newNode
      }
    }
    index.get(t) match {
      case Some(existingNode) => existingNode.children.add(child)
      case None               => index.addOne(t -> Node(t, mutable.HashSet(child)))
    }
  }
}
