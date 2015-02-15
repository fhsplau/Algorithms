package io.paverell.datastructures.BST

// TODO add constructor
// TODO add Node to non-empty tree
// TODO add Node to empty tree
// TODO sort
// TODO print => toString
// TODO delete element
// TODO merge trees => try to do something bether than O(n)
// TODO change to funcional programming
// TODO is balanced
// TODO do balance
// TODO get method => add keys => O(n) or bether
// TODO flatt

abstract class BSTree {
  def add(e: Int): BSTree

  def contains(e: Int): Boolean
}

class EmptyNode extends BSTree {

  override def add(element: Int): BSTree = new Node(element, new EmptyNode, new EmptyNode)

  override def contains(element: Int): Boolean = false

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
