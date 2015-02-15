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
    case i if i == priority => true
    case i if i < priority => leftQueue.contains(i)
    case i if i > priority => rightQueue.contains(i)
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

  def insert(node: NonEmptyQueue): Unit =
    if (rightQueue.asInstanceOf[NonEmptyQueue].priority == node.priority) {
      rightQueue = node.leftQueue
    } else rightQueue.asInstanceOf[NonEmptyQueue].insert(node)

  override def toString = "{" + leftQueue + priority + rightQueue + "}"
}

class PriorityQueue(maxSize: Int) {

  private var head: BSTQueue = new EmptyQueue

  private var s = 0

  def highestPriority = if (head.isEmpty) 0 else head.asInstanceOf[NonEmptyQueue].highestPriorityInNode

  def size: Int = s

  def isEmpty: Boolean = head.isEmpty

  def pop: Int = {
    val node = head.pop(highestPriority)
    
    if (head.asInstanceOf[NonEmptyQueue].priority == highestPriority)
      head = node.asInstanceOf[NonEmptyQueue].leftQueue
    else
      head.asInstanceOf[NonEmptyQueue].insert(node.asInstanceOf[NonEmptyQueue])

    s = s - 1
    node.asInstanceOf[NonEmptyQueue].task
  }

  def push(priority: Int, task: Int): PriorityQueue =
    if (s > maxSize - 1) throw new IndexOutOfBoundsException
    else {
      head = head.push(priority, task)
      s += 1
      this
    }

  def contains(priority: Int): Boolean = head.contains(priority)

  override def toString = 
    if (!head.isEmpty) head.asInstanceOf[NonEmptyQueue].toString
    else head.asInstanceOf[EmptyQueue].toString
}
