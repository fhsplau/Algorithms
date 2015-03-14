package io.paverell.algorithms.sort

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class MergeSortTest extends FunSuite with BeforeAndAfter{
  var ms: MergeSort = _
  
  before {
    ms = new MergeSort
  }

  test("if list is empty sort returns empty list") {
    assert(ms.sort(List()) === List())
  }

  test("sort non empty list") {
    assert(ms.sort(List(1)) === List(1))
    assert(ms.sort(List(2,1)) === List(1,2))
    assert(ms.sort(List(6, 4, 1)) === List(1, 4, 6))
  }

  test("should return sorted list if input is sorted") {
    assert(ms.sort(List(1, 2, 3)) === List(1, 2, 3))
  }

  test("duplicates in the list") {
    assert(ms.sort(List(6, 4, 1, 6, 2, 1)) === List(1, 1, 2, 4, 6, 6))
  }

  test("huge array") {
    val n = 10000
    val a: Seq[Int] = 1 to n map { _ => Random.nextInt(n-1)}
    val array: List[Int] = List(a: _*)

    assert(ms.sort(array) === array.sorted)
  }

}
