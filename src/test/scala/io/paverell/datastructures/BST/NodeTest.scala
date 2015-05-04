package io.paverell.datastructures.BST

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class NodeTest extends FunSuite with BeforeAndAfter {
  var node: BSTree = _

  before {
    node = new Node(10, new EmptyNode, new EmptyNode)
  }

  test("can add element to a node") {
    node = addAll(List(8, 11), node)
    assertContainsAll(List(10, 8, 11), node)
  }

  test("node contains element") {
    assert(node.contains(10))
  }

  test("can remove from left subtree") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    node.remove(8)
    assertContainsAll(List(10, 12, 6, 9, 13, 11), node)
    assert(!node.contains(8))
  }

  test("can retrieve min value from node") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    assert(node.min === 6)
  }

  test("can retrieve max value from node") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    assert(node.max === 13)
  }

  test("node is a leaf if it don't has left and right subtree") {
    assert(node.isLeaf)
  }

  test("node is not a leaf if it has left or right subtree") {
    node = node.add(12)
    assertContainsAll(List(10, 12), node)
    assert(!node.isLeaf)
  }

  def assertContainsAll(elements: List[Int], node: BSTree): Unit =
    assert(elements.map(x => node.contains(x)).reduceLeft(_ && _))

  def addAll(elements: List[Int], node: BSTree): BSTree = {
    if (elements.isEmpty) node else addAll(elements.tail, node.add(elements.head))
  }
}
