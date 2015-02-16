package io.paverell.datastructures.queues

trait Queue {

  def push(priority: Int, task: Int): BSTQueue

}

abstract class BSTQueue extends Queue {
  def pop(p: Int): (Int, BSTQueue)

  def contains(priority: Int): Boolean

  def highestPriority: Option[Int]

  val isEmpty: Boolean
}

class EmptyQueue extends BSTQueue {
  override def pop(p: Int): (Int, BSTQueue) = throw new RuntimeException("EMPTY QUEUE")

  override def push(priority: Int, task: Int): BSTQueue = new NonEmptyQueue(
    priority,
    task,
    new EmptyQueue,
    new EmptyQueue
  )

  override def highestPriority: Option[Int] = null

  override val isEmpty = true

  override def contains(priority: Int): Boolean = false

  override def toString = "."
}

case class NonEmptyQueue(priority: Int, task: Int, var rightQueue: BSTQueue, var leftQueue: BSTQueue) extends BSTQueue {

  override val isEmpty = false

  override def pop(p: Int): (Int, BSTQueue) = {
    def retrieveNodeWithHighestPriority(node: NonEmptyQueue): NonEmptyQueue = {
      if (node.priority == p) node
      else retrieveNodeWithHighestPriority(node.rightQueue.asInstanceOf[NonEmptyQueue])
    }

    def insert(node: NonEmptyQueue, q: BSTQueue): BSTQueue = {

      def insertNode(rq: BSTQueue): BSTQueue = {
        val queue = rq.asInstanceOf[NonEmptyQueue]
        if (queue.priority == node.priority)
          rightQueue = node.leftQueue
        else insertNode(queue.rightQueue)

        this
      }

      def replaceRoot(root: NonEmptyQueue): BSTQueue = {
        if (root.leftQueue.isEmpty) new EmptyQueue
        else {
          val left = root.leftQueue.asInstanceOf[NonEmptyQueue]
          new NonEmptyQueue(left.priority, left.task, left.rightQueue, left.leftQueue)
        }
      }

      if(node.priority==priority) replaceRoot(node) else insertNode(q)
    }

    val nodeWithHighestPriority = retrieveNodeWithHighestPriority(this)

    (nodeWithHighestPriority.task, insert(nodeWithHighestPriority,this))
  }

  override def push(p: Int, t: Int): BSTQueue = {
    if (p < priority) new NonEmptyQueue(priority, task, rightQueue, leftQueue.push(p, t))
    else if (p > priority) new NonEmptyQueue(priority, task, rightQueue.push(p, t), leftQueue)
    else this
  }

  override def contains(p: Int): Boolean = p match {
    case i if i < priority => leftQueue.contains(i)
    case i if i > priority => rightQueue.contains(i)
    case _ => true
  }

  def highestPriority: Option[Int] = {
    def impl(q: BSTQueue, prevPriority: Int): Int =
      if (q.isEmpty) prevPriority
      else impl(q.asInstanceOf[NonEmptyQueue].rightQueue,
        if (prevPriority > q.asInstanceOf[NonEmptyQueue].priority) prevPriority
        else q.asInstanceOf[NonEmptyQueue].priority
      )

    Some(impl(rightQueue, priority))
  }

  override def toString = "{" + leftQueue + priority + rightQueue + "}"
}

class PriorityQueue(maxSize: Int) {

  private var queue: BSTQueue = new EmptyQueue

  private var s = 0

  var currentTask: Option[Int] = null

  def highestPriority: Option[Int] = queue.highestPriority

  def size: Int = s

  def isEmpty: Boolean = queue.isEmpty

  def pop: Int = {
    val r = queue.pop(highestPriority.getOrElse(0))
    queue = r._2
    s = if (s < 0) 0 else s - 1
    r._1
  }

  def push(priority: Int, task: Int): PriorityQueue =
    if (s > maxSize - 1) throw new IndexOutOfBoundsException
    else {
      queue = queue.push(priority, task)
      s += 1
      this
    }

  def contains(priority: Int): Boolean = queue.contains(priority)

  override def toString =
    if (!queue.isEmpty) queue.asInstanceOf[NonEmptyQueue].toString
    else queue.asInstanceOf[EmptyQueue].toString
}
