package io.paverell.datastructures.BST


abstract class BSTree {
  def add(e: Int): BSTree

  def contains(e: Int): Boolean

  def remove(e: Int): BSTree

  def min: Int

  def max: Int

  def isLeaf: Boolean

  def isEmpty: Boolean
}

class EmptyNode extends BSTree {

  override def add(element: Int): BSTree = new Node(element, new EmptyNode, new EmptyNode)

  override def contains(element: Int): Boolean = false

  override def remove(e: Int): BSTree = this

  override def min: Int = ???

  override def max: Int = ???

  override def isLeaf: Boolean = true

  override def isEmpty: Boolean = true

  override def toString = "."
}

class Node(e: Int, left: BSTree, right: BSTree) extends BSTree {
  override def add(element: Int): BSTree =
    if (element > e) new Node(e, left, right.add(element))
    else if (element < e) new Node(e, left.add(element), right)
    else this

  override def contains(element: Int): Boolean =
    if (element > e) right.contains(element)
    else if (element < e) left.contains(element)
    else true

  override def remove(element: Int): BSTree = ???

  override def min: Int = if (left.isEmpty) e else left.min

  override def max: Int = if (right.isEmpty) e else right.max

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def isEmpty: Boolean = false

  override def toString = "{" + left + e + right + "}"
}

class BST {

  private var root: BSTree = null

  def isEmpty: Boolean = root == null

  def contains(element: Int) = root.contains(element)

  def add(element: Int): BSTree = if (root == null) {
    root = new Node(element, new EmptyNode, new EmptyNode)
    root
  } else {
    root = root.add(element)
    root
  }

}
