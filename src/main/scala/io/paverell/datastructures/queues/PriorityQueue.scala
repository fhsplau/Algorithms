package io.paverell.datastructures.queues

trait Queue {

  def push(priority: Int, task: Int): BSTQueue

}

abstract class BSTQueue extends Queue {
  def pop(priority: Int): (Int, BSTQueue)

  def contains(priority: Int): Boolean

  def highestPriority: Option[Int]

  val isEmpty: Boolean
}

class EmptyQueue extends BSTQueue {
  override def pop(priority: Int): (Int, BSTQueue) = throw new RuntimeException("EMPTY QUEUE")

  override def push(priority: Int, task: Int): BSTQueue = new NonEmptyQueue(priority, task, new EmptyQueue, new EmptyQueue)

  override def highestPriority: Option[Int] = null

  override val isEmpty = true

  override def contains(priority: Int): Boolean = false

  override def toString = "."
}

case class NonEmptyQueue(nodePriority: Int, task: Int, var rightQueue: BSTQueue, var leftQueue: BSTQueue) extends BSTQueue {

  override val isEmpty = false

  override def pop(priority: Int): (Int, BSTQueue) = {
    def getNodeWithHighestPriority(queue: NonEmptyQueue): NonEmptyQueue = {
      if (queue.nodePriority == priority) queue
      else getNodeWithHighestPriority(queue.rightQueue.asInstanceOf[NonEmptyQueue])
    }

    val nwhp = getNodeWithHighestPriority(this)

    def insertHPInto(queue: NonEmptyQueue): BSTQueue = {

      def insertNode(rq: NonEmptyQueue): BSTQueue = {
        if (rq.nodePriority == nwhp.nodePriority)
          rightQueue = nwhp.leftQueue
        else insertNode(rq.rightQueue.asInstanceOf[NonEmptyQueue])

        this
      }

      def replaceRoot(root: NonEmptyQueue): BSTQueue = {
        if (root.leftQueue.isEmpty) new EmptyQueue
        else {
          val left = root.leftQueue.asInstanceOf[NonEmptyQueue]
          new NonEmptyQueue(left.nodePriority, left.task, left.rightQueue, left.leftQueue)
        }
      }

      if (nwhp.nodePriority == nodePriority) replaceRoot(nwhp) else insertNode(queue)
    }

    (nwhp.task, insertHPInto(this))
  }

  override def push(p: Int, t: Int): BSTQueue = {
    if (p < nodePriority) new NonEmptyQueue(nodePriority, task, rightQueue, leftQueue.push(p, t))
    else if (p > nodePriority) new NonEmptyQueue(nodePriority, task, rightQueue.push(p, t), leftQueue)
    else this
  }

  override def contains(p: Int): Boolean = p match {
    case i if i < nodePriority => leftQueue.contains(i)
    case i if i > nodePriority => rightQueue.contains(i)
    case _ => true
  }

  def highestPriority: Option[Int] = {
    def impl(q: BSTQueue, prevPriority: Int): Int =
      if (q.isEmpty) prevPriority
      else impl(q.asInstanceOf[NonEmptyQueue].rightQueue,
        if (prevPriority > q.asInstanceOf[NonEmptyQueue].nodePriority) prevPriority
        else q.asInstanceOf[NonEmptyQueue].nodePriority
      )

    Some(impl(rightQueue, nodePriority))
  }

  override def toString = "{" + leftQueue + nodePriority + rightQueue + "}"
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
