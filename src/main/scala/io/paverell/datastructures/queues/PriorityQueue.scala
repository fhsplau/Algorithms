package io.paverell.datastructures.queues

case class BSTQueue[T](priority: Int, task: T) {
  var right: BSTQueue[T] = null
  var left: BSTQueue[T] = null

  def push: (Int, T) => BSTQueue[T] = (p: Int, t: T) => {
    if (p > priority)
      if(right == null) right = new BSTQueue[T](p, t)
      else right.push(p, t)
    else if (p < priority)
      if(left==null) left = new BSTQueue(p, t)
      else left.push(p, t)

    this
  }

  def pop: Int => (T, BSTQueue[T]) = (hp: Int) =>
    if (hp == priority) (task, left)
    else if (hp == right.priority) {
      val tmp = right.task
      right = right.left
      (tmp, this)
    }
    else right.pop(hp)

  def contains: Int => Boolean = (p: Int) =>
    if (p < priority) if(left == null) false else left.contains(p)
    else if (p > priority) if(right == null) false else right.contains(p)
    else true

  def highestPriority: Int =
    if (right == null) priority
    else right.highestPriority

  def lowestPriority: Int =
    if (left == null) priority
    else left.lowestPriority

}

class PriorityQueue[T](maxSize: Int) {

  type Task = () => T

  private var root: BSTQueue[T] = null

  private var s = 0

  def highestPriority = if (root == null) null else root.highestPriority

  def lowestPriority = if (root == null) null else root.lowestPriority

  def size = s

  def isEmpty = if (root == null) true else false

  def pop: Task = () =>
    if (root == null) throw new RuntimeException
    else {
      val tmp = root.pop(root.highestPriority)
      s = s - 1
      root = tmp._2
      tmp._1
    }

  def push: (Int, T) => PriorityQueue[T] = (priority: Int, task: T) =>
    if (size == maxSize) throw new IndexOutOfBoundsException
    else {
      if (root != null) root = root.push(priority, task)
      else root = new BSTQueue[T](priority, task)

      s = s + 1

      this
    }

  def contains: Int => Boolean = (priority: Int) =>
    if (root == null) false
    else root.contains(priority)
}