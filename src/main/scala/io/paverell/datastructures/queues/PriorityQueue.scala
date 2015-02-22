package io.paverell.datastructures.queues

case class BSTQueue(priority: Int, task: Int) {
  var right: BSTQueue = null
  var left: BSTQueue = null

  def push: (Int, Int) => BSTQueue = (p: Int, t: Int) => {
    if (p > priority && right == null) right = new BSTQueue(p, t)
    else if (p < priority && left == null) left = new BSTQueue(p, t)
    else if (p > priority) right.push(p, t)
    else left.push(p, t)

    this
  }

  def pop: Int => (Int, BSTQueue) = (hp: Int) =>
    if (hp == priority) (task, left)
    else if (hp == right.priority) {
      val tmp = right.task
      right = right.left
      (tmp, this)
    }
    else right.pop(hp)

  def contains: Int => Boolean = (p: Int) => 
    if (p == priority) true
    else if (p < priority && left == null) false
    else if (p > priority && right == null) false
    else if (p > priority) right.contains(p)
    else left.contains(p)

  def highestPriority: Int =
    if (right == null) priority
    else right.highestPriority

  def lowestPriority: Int =
    if (left == null) priority
    else left.lowestPriority

}

class PriorityQueue(maxSize: Int) {

  type Task = () => Int

  private var root: BSTQueue = null

  private var s = 0

  def highestPriority = if (root == null) null else root.highestPriority

  def lowestPriority = if(root == null) null else root.lowestPriority

  def size = s

  def isEmpty = if(root == null) true else false

  def pop: Task = () =>
    if (root == null) throw new RuntimeException
    else {
      val tmp = root.pop(root.highestPriority)
      s = s - 1
      root = tmp._2
      tmp._1
    }

  def push: (Int, Int) => PriorityQueue = (priority: Int, task: Int) =>
    if (size == maxSize) throw new IndexOutOfBoundsException
    else {
      if (root != null) root = root.push(priority, task)
      else root = new BSTQueue(priority, task)

      s = s + 1

      this
    }

  def contains: Int => Boolean = (priority: Int) =>
    if (root == null) false
    else root.contains(priority)

}