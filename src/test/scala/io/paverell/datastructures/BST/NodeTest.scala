package io.paverell.datastructures.BST

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.Matchers._

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
    node.contains(10) shouldBe true
  }

  test("can remove from left subtree") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    node = node.remove(8)
    assertContainsAll(List(10, 12, 6, 9, 13, 11), node)
    node.contains(8) should not be true
  }

  test("can remove from right subtree") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    node = node.remove(12)
    assertContainsAll(List(10, 13, 6, 9, 8, 11), node)
    node.contains(12) should not be true
  }

  test("can remove a leaf") {
    node = node.remove(10)
    node.isEmpty shouldBe true
  }

  test("can retrieve min value from node") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    node.min shouldEqual Some(6)
  }

  test("can retrieve max value from node") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    node.max shouldEqual Some(13)
  }

  test("node is a leaf if it don't has left and right subtree") {
    node.isLeaf shouldBe Some(true)
  }

  test("node is not a leaf if it has left or right subtree") {
    node = node.add(12)
    val node2 = new NonEmptyNode(11, new NonEmptyNode(9, EmptyNode, EmptyNode), EmptyNode)
    assertContainsAll(List(10, 12), node)
    node.isLeaf should not be Some(true)
    node2.isLeaf should not be Some(true)
  }

  test("can retrieve size from a node") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    node.size shouldEqual 7
  }

  test("size of left and right subtrees is smaller than roots size") {
    node = addAll(List(8, 12, 6, 9, 13, 11), node)
    getNonEmptyNode(node).right.size should be < node.size
    getNonEmptyNode(node).left.size should be < node.size
  }

  def getNonEmptyNode(node: Node): NonEmptyNode = node.asInstanceOf[NonEmptyNode]

  def assertContainsAll(elements: List[Int], node: Node): Unit =
    elements.map(x => node.contains(x)).reduceLeft(_ && _) shouldBe true

  def addAll(elements: List[Int], node: Node): Node = {
    if (elements.isEmpty) node else addAll(elements.tail, node.add(elements.head))
  }
}
