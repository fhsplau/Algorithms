package io.paverell.algorithms.sort

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class InsertionSortTest extends FunSuite with BeforeAndAfter {
  var is: InsertionSort = _

  before {
    is = new InsertionSort
  }

  test("if list is empty sort returns empty list") {
    assert(is.sort(List()) === List())
  }
}
