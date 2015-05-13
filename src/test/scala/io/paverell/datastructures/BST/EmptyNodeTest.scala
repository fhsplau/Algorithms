package io.paverell.datastructures.BST

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._


@RunWith(classOf[JUnitRunner])
class EmptyNodeTest extends FunSuite with BeforeAndAfter{
  var node: Node = _

  before {
    node = EmptyNode
  }

  test("after adding to EmptyNode it returns NonEmpty"){
    node = node.add(10)
    node.isEmpty should not be true
  }

  test("Empty node not contains any value") {
    node.contains(0) shouldBe false
  }

  test("Can't remove from empty node") {
    val thrown = intercept[IndexOutOfBoundsException]{
      node remove 10
    }

    thrown.getMessage shouldEqual "empty node"
  }

  test("Min/Max value in Empty node equals to None") {
    node.min shouldBe None
    node.max shouldBe None
  }

  test("Empty node size equal to 0") {
    node.size shouldEqual 0
  }

  test("Empty node is empty"){
    node.isEmpty shouldBe true
  }

  test("isLeaf for empty node returns None") {
    node.isLeaf shouldBe None
  }
}
