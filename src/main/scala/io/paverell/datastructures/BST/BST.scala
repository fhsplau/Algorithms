package io.paverell.datastructures.BST


sealed trait Node {
  def add: Int => Node

  def contains: Int => Boolean

  def remove: Int => Node

  def min: Option[Int]

  def max: Option[Int]

  def isLeaf: Boolean

  def isEmpty: Boolean

  val size: Int
}

object EmptyNode extends Node {

  override def add: Int => Node = element => new NonEmptyNode(element, EmptyNode, EmptyNode)

  override def contains: Int => Boolean = e => false

  override def remove: Int => Node = element => this

  override def min: Option[Int] = None

  override def max: Option[Int] = None

  override def isLeaf: Boolean = true

  override def isEmpty: Boolean = true

  override def toString = "."

  override val size: Int = 0
}

case class NonEmptyNode(root: Int, left: Node, right: Node) extends Node {
  override def add: Int => Node = e =>
    if (e > root) new NonEmptyNode(root, left, right.add(e))
    else new NonEmptyNode(root, left.add(e), right)

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
    else if (!left.isEmpty && right.isEmpty) new NonEmptyNode(left.max.get, left.remove(left.max.get), EmptyNode)
    else if (!right.isEmpty && left.isEmpty) new NonEmptyNode(right.min.get, EmptyNode, right.remove(right.min.get))
    else new NonEmptyNode(left.max.get, left.remove(left.max.get), right)

  override def min: Option[Int] = if (left.isEmpty) Option(root) else left.min

  override def max: Option[Int] = if (right.isEmpty) Option(root) else right.max

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

  override def isLeaf: Boolean = root.isLeaf

  override def max: Option[Int] = root.max

  override def remove: (Int) => Node = e => root.remove(e)

  override def min: Option[Int] = root.min

  override def contains: Int => Boolean = element => root.contains(element)

  override val size: Int = root.size
}
