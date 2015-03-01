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

  test("sort non empty list") {
    assert(is.sort(List(6, 4, 1)) === List(1, 4, 6))
  }

  test("should return sorted list if input is sorted") {
    assert(is.sort(List(1, 2, 3)) === List(1, 2, 3))
  }

  test("more than one the same int in the list") {
    assert(is.sort(List(6, 4, 1, 6, 2, 1)) === List(1, 1, 2, 4, 6, 6))
  }
}
