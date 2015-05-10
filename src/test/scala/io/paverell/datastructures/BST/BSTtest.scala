package io.paverell.datastructures.BST

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class BSTtest extends FunSuite with BeforeAndAfter {

  var tree: Node = _

  before {
    tree = new BST
  }

  test("new tree is empty") {
    assert(tree.isEmpty)
  }

  test("can add to the tree") {
    tree = tree.add(1)
    assert(tree.contains(1))
  }

  test("false if nonempty tree those not contains element") {
    tree.add(1)
    assert(!tree.contains(2))
  }

  test("add element to nonempty tree") {
    tree = tree.add(1).add(2)
    assert(tree.contains(2))
  }

  test("add to left subtree") {
    tree = tree.add(2).add(3).add(1)

    assert(tree.contains(1))
  }

  test("add elements to non empty tree and check if it does not contain non existing element") {
    tree = tree.add(2).add(3).add(1)

    assert(tree.contains(2))
    assert(!tree.contains(4))
  }

  test("can remove element from a tree"){
    tree.add(10)
    tree.add(3)
    tree.add(1)

  }

}
