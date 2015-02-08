package io.paverell.datastructures.BST

abstract class BSTree {
  def add(e: Int): BSTree

  def contains(e: Int): Boolean
}

class EmptyNode extends BSTree {

  override def add(element: Int): BSTree = new Node(element, new EmptyNode, new EmptyNode)

  override def contains(element: Int): Boolean = false

}

class Node(e: Int, left: BSTree, right: BSTree) extends BSTree {
  override def add(element: Int): BSTree = element match {
    case i if i > e => right.add(i)
    case i if i < e => left.add(i)
  }

  override def contains(element: Int): Boolean = element match {
    case i if i == e => true
    case i if i > e => right.contains(i)
    case i if i < e => left.contains(i)
  }
}


class BST extends BSTree {

  private var root: BSTree = new EmptyNode

  override def contains(element: Int) = root.contains(element)

  override def add(element: Int): BSTree = {
    root = root.add(element)
    root
  }

}
