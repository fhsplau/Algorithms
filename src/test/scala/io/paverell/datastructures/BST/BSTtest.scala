package io.paverell.datastructures.BST
import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BSTtest extends FunSuite with BeforeAndAfter{

  var tree: BST = _

  before {
    tree = new BST
  }

  test("new tree is empty") {
    assert(!tree.contains(0))
  }

  test("can add to the tree") {
    tree.add(1)
    assert(tree.contains(1))
  }

  test("false if nonempty tree those not contains element") {
    tree.add(1)
    assert(!tree.contains(2))
  }

  test("add element to nonempty tree") {
    tree.add(1)
    tree.add(2)
    assert(tree.contains(2))
  }

  test("add smaller element to non empty tree") {
    tree.add(2)
    tree.add(3)
    tree.add(1)

    assert(tree.contains(1))
  }

  test("add elements to non empty tree and check if it do not contains non existing element"){
    tree.add(2)
    tree.add(3)
    tree.add(1)

    assert(!tree.contains(4))
  }

}
