package io.paverell.datastructures.queues

trait Queue {

  def pop(p: Int): BSTQueue

  def push(priority: Int, task: Int): BSTQueue

}

abstract class BSTQueue extends Queue {
  def contains(priority: Int): Boolean

  val isEmpty: Boolean
}

class EmptyQueue extends BSTQueue {
  override def pop(p: Int): BSTQueue = throw new RuntimeException("EMPTY QUEUE")

  override def push(priority: Int, task: Int): BSTQueue = new NonEmptyQueue(
    priority,
    task,
    new EmptyQueue,
    new EmptyQueue
  )

  override val isEmpty = true

  override def contains(priority: Int): Boolean = false

  override def toString = "."
}

case class NonEmptyQueue(priority: Int, task: Int, var rightQueue: BSTQueue, var leftQueue: BSTQueue) extends BSTQueue {

  override val isEmpty = false

  override def pop(p: Int): BSTQueue = if (priority == p) this else rightQueue.pop(p)

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

  def highestPriorityInNode: Int = {
    def impl(q: BSTQueue, prevPriority: Int): Int =
      if (q.isEmpty) prevPriority
      else impl(q.asInstanceOf[NonEmptyQueue].rightQueue,
        if (prevPriority > q.asInstanceOf[NonEmptyQueue].priority) prevPriority
        else q.asInstanceOf[NonEmptyQueue].priority
      )

    impl(rightQueue, priority)
  }

  def insert(node: NonEmptyQueue): BSTQueue = {

    def impl: BSTQueue = {
      val right = rightQueue.asInstanceOf[NonEmptyQueue]
      if (right.priority == node.priority)
        rightQueue = node.leftQueue
      else right.insert(node)

      this
    }

    node match {
      case i if i.priority == priority => if (i.leftQueue.isEmpty) new EmptyQueue
      else {
        val nodeLeft = i.leftQueue.asInstanceOf[NonEmptyQueue]
        new NonEmptyQueue(nodeLeft.priority, nodeLeft.task, nodeLeft.rightQueue, nodeLeft.leftQueue)
      }
      case _ => impl
    }
  }

  override def toString = "{" + leftQueue + priority + rightQueue + "}"
}

class PriorityQueue(maxSize: Int) {

  private var queue: BSTQueue = new EmptyQueue

  private var s = 0

  def highestPriority: Option[Int] =
    if (queue.isEmpty) null
    else Some(queue.asInstanceOf[NonEmptyQueue].highestPriorityInNode)

  def size: Int = s

  def isEmpty: Boolean = queue.isEmpty

  def pop: Int = {
    val node = queue.pop(highestPriority.getOrElse(0)).asInstanceOf[NonEmptyQueue]
    queue = queue.asInstanceOf[NonEmptyQueue].insert(node)
    s = if (s < 0) 0 else s - 1
    node.task
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
