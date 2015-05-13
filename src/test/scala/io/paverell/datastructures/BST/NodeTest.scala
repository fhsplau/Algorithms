package io.paverell.datastructures.BST

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class NodeTest extends FunSuite with BeforeAndAfter {
  var node: Node = _

  before {
    node = new NonEmptyNode(10, EmptyNode, EmptyNode)
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
    node = node.remove(8)
    assertContainsAll(List(10, 12, 6, 9, 13, 11), node)
    assert(!node.contains(8))
  }

  test("can remove from right subtree") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    node = node.remove(12)
    assertContainsAll(List(10, 13, 6, 9, 8, 11), node)
    assert(!node.contains(12))
  }

  test("can remove a leaf") {
    node = node.remove(10)
    assert(node.isEmpty)
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
    val node2 = new NonEmptyNode(11, new NonEmptyNode(9, EmptyNode, EmptyNode), EmptyNode)
    assertContainsAll(List(10, 12), node)
    assert(!node.isLeaf)
    assert(!node2.isLeaf)
  }

  test("can retrieve size from a node") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    assert(node.size == 7)
  }

  def assertContainsAll(elements: List[Int], node: Node): Unit =
    assert(elements.map(x => node.contains(x)).reduceLeft(_ && _))

  def addAll(elements: List[Int], node: Node): Node = {
    if (elements.isEmpty) node else addAll(elements.tail, node.add(elements.head))
  }
}
