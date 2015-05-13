package io.paverell.datastructures.BST


trait Node {
  def add: Int => Node

  def contains: Int => Boolean

  def remove: Int => Node

  def min: Int

  def max: Int

  def isLeaf: Boolean

  def isEmpty: Boolean

  val size: Int
}

object EmptyNode extends Node {

  override def add: Int => Node = element => new NonEmptyNode(element, EmptyNode, EmptyNode)

  override def contains: Int => Boolean = e => false

  override def remove: Int => Node = element => this

  override def min: Int = ???

  override def max: Int = ???

  override def isLeaf: Boolean = true

  override def isEmpty: Boolean = true

  override def toString = "."

  override val size: Int = 0
}

class NonEmptyNode(root: Int, left: Node, right: Node) extends Node {
  override def add: Int => Node = e =>
    if (e > root) new NonEmptyNode(root, left, right.add(e))
    else if (e < root) new NonEmptyNode(root, left.add(e), right)
    else this

  override def contains: Int => Boolean = e =>
    if (e > root) right.contains(e)
    else if (e < root) left.contains(e)
    else true

  override def remove: Int => Node = {
    case i if i > root => new NonEmptyNode(root, left, right.remove(i))
    case i if i < root => new NonEmptyNode(root, left.remove(i), right)
    case i if i == root => removeElement()
  }

  private val removeElement: () => Node = () =>
    if (this.isLeaf) EmptyNode
    else if (!left.isEmpty && right.isEmpty) new NonEmptyNode(left.max, left.remove(left.max), EmptyNode)
    else if (!right.isEmpty && left.isEmpty) new NonEmptyNode(right.min, EmptyNode, right.remove(right.min))
    else new NonEmptyNode(left.max, left.remove(left.max), right)

  override def min: Int = if (left.isEmpty) root else left.min

  override def max: Int = if (right.isEmpty) root else right.max

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def isEmpty: Boolean = false

  override def toString = "{" + left + root + right + "}"

  override val size: Int = left.size + 1 + right.size
}

class BST extends Node {

  private val root: Node = EmptyNode

  def isEmpty: Boolean = root.isEmpty

  override def add: (Int) => Node = e =>
    if (root.isEmpty) new NonEmptyNode(e, EmptyNode, EmptyNode)
    else root.add(e)

  override def isLeaf: Boolean = ???

  override def max: Int = ???

  override def remove: (Int) => Node = ???

  override def min: Int = ???

  override def contains: Int => Boolean = element => root.contains(element)

  override val size: Int = root.size
}
